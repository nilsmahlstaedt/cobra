package net.flatmap.cobra

import net.flatmap.collaboration.{Annotations, ClientInterface, EditorInterface, Operation}
import net.flatmap.js.codemirror._
import net.flatmap.js.reveal.Reveal
import org.scalajs.dom.{Element, console, raw}
import net.flatmap.js.util._
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.raw.HTMLElement

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js._
import scala.util.control.NonFatal
import scala.util.matching.Regex

/**
  * Created by martin on 12.02.16.
  */
object Code {
  def loadDelayed(root: NodeSeqQuery): Future[Seq[String]] = Future.sequence {
    root.query(s"code[src]:not([src^='#'])").elements.map { code =>
      val src = code.getAttribute("src")
      val ext = src.split("\\.").last
      Mode.modes.find(_.fileendings.contains(ext)).foreach(code.classes += _.name)
      code.text = Ajax.get(src).filter(_.status == 200).map(_.responseText).recover {
        case NonFatal(e) =>
          console.error(s"could not load source from '$src'")
          s"could not load source from '$src'"
      }
    }
  }

  def stripIndentation(raw: String): String = {
    val strippedFront = raw.lines.dropWhile(!_.exists(!_.isWhitespace)).toSeq
    strippedFront.headOption.fold("") { firstLine =>
      val s = strippedFront.map(_.stripPrefix(firstLine.takeWhile(_.isWhitespace))).mkString("\n")
      s.take(s.lastIndexWhere(!_.isWhitespace) + 1)
    }
  }

  def subdocuments(root: Doc, Regex: Regex): Map[String,Doc] = {
    var line = root.firstLine()
    var starts = Map.empty[String,Int]
    var ends = Map.empty[String,Int]
    while (line < root.lineCount()) {
      root.getLine(line) match {
        case Regex("begin",name) =>
          starts += name -> line
          root.replaceRange("",CodeMirror.Pos(line,0),CodeMirror.Pos(line + 1, 0))
        case Regex("end",name) =>
          ends += name -> line
          root.replaceRange("",CodeMirror.Pos(line,0),CodeMirror.Pos(line + 1, 0))
        case other =>
          line += 1
      }
    }
    starts.collect {
      case (name,sl) if ends.contains(name) =>
        val el = ends(name)
        val doc = root.linkedDoc(new LinkedDocOptions(from = sl, to = el, sharedHist = true))
        doc.on("beforeChange",(x: Doc, y: EditorChange) => CodeMirror.signal(root, "beforeChange", root, y))
        doc.on("cursorActivity",(x: Doc) => CodeMirror.signal(root, "cursorActivity", x))
        name -> doc
    }
  }

  def mode(code: Element): Mode =
    Mode.modes.find(mode => code.classes.contains(mode.name)).getOrElse(Plain)

  def attachDocument(id: String, doc: Doc, mode: Mode) = {
    CobraJS.send(InitDoc(id,doc.getValue(), mode))

    var silent = false
    def silently[T](f: => T): T = {
      silent = true; val t = f; silent = false; t
    }

    val removeAnnotations = mutable.Map.empty[String,() => Unit].withDefaultValue(() => ())

    val editorInterface = new EditorInterface[Char] {
      def applyOperation(operation: Operation[Char]) =
        silently(CodeMirrorOps.applyOperation(doc,operation))
      def sendOperation(operation: Operation[Char], revision: Long) =
        CobraJS.send(Edit(id,operation,revision))
      def sendAnnotations(aid: String, annotations: Annotations, revision: Long) =
        CobraJS.send(Annotate(id,aid,annotations,revision))
      def applyAnnotations(aid: String, annotations: Annotations) = {
        removeAnnotations(aid)()
        removeAnnotations(aid) = CodeMirrorOps.applyAnnotations(doc, annotations,mode)
      }
    }

    var client = ClientInterface[Char](editorInterface)

    var hoverInfo = mutable.Buffer.empty[Clearable]

    CobraJS.listenOn(id) {
      case AcknowledgeEdit(_) => client.serverAck()
      case RemoteEdit(_,op) => client.remoteEdit(op)
      case CombinedRemoteEdit(_,op,revs) =>
        client.combinedRemoteEdit(op,revs)
      case ResetSnippet(_,content,rev) =>
        console.warn(s"snippet $id was resetted by the server")
        client.reset(rev)
        silently(doc.setValue(content))
      case RemoteAnnotations(_,aid,annotations) =>
        client.remoteAnnotations(aid,annotations)
      case Information(_,from,to,body) =>
        val root = doc
        def widget(doc: Doc): Unit = if (doc.getEditor() != js.undefined) {
          val f = root.posFromIndex(from)
          val pos = root.posFromIndex(to)
          if (doc.firstLine() <= pos.line && doc.lastLine() >= pos.line) {
            import net.flatmap.js.codemirror.plugins.Runmode._
            val elem = org.scalajs.dom.document.createElement("div").asInstanceOf[HTMLElement]
            elem.classes += "info"
            elem.classes += mode.name
            if (mode == Isabelle) {
              elem.innerHTML = body
            } else {
              CodeMirror.runMode(body,mode.mime,elem)
              elem.classes += "pre"
            }
            hoverInfo.foreach(_.clear())
            hoverInfo.clear()
            hoverInfo += doc.getEditor().addLineWidget(pos.line, elem)
            val options = TextMarkerOptions()
            options.className = "hoverInfo"
            hoverInfo += doc.markText(f,pos,options)
          }
        }
        widget(root)
        root.iterLinkedDocs((doc: Doc, bool: Boolean) => widget(doc))
    }

    val changeHandler: js.Function2[Doc,EditorChange,Unit] =
      (doc: Doc, e: EditorChange) => if (!silent) {
        client.localEdit(CodeMirrorOps.changeToOperation(doc,e))
      }



    val selectHandler: js.Function1[Doc,Unit] = {
      var next = Option.empty[Option[RequestInfo]]
      val root = doc
      (doc: Doc) => {
        hoverInfo.foreach(_.clear())
        hoverInfo.clear()
        console.log("selection Change")
        doc.listSelections().filter(r => r.head.line != r.anchor.line || r.head.ch != r.anchor.ch).foreach { range =>
          val req = if (range.anchor.line < range.head.line || range.anchor.line == range.head.line && range.anchor.ch < range.head.ch)
            RequestInfo(id, root.indexFromPos(range.anchor), root.indexFromPos(range.head))
          else
            RequestInfo(id, root.indexFromPos(range.head), root.indexFromPos(range.anchor))
          if (next.isEmpty) {
            CobraJS.send(req)
            next = Some(None)
            scheduleOnce(500) {
              next.foreach(_.foreach(CobraJS.send))
              next = None
            }
          } else {
            next = Some(Some(req))
          }
        }
      }
    }

    doc.on("beforeChange",changeHandler)
    doc.on("cursorActivity",selectHandler)
  }

  def initializeDocuments(root: NodeSeqQuery): Map[String,Doc] = {
    var lastSuccessfulSuffix = 0

    def nextIds = {
      Iterator.iterate(lastSuccessfulSuffix) { x =>
        lastSuccessfulSuffix = x + 1
        lastSuccessfulSuffix
      }.map("code_snippet_" + _).drop(1)
    }

    def nextId = nextIds.find(id => $"#$id".elements.isEmpty).get

    root.query("code").elements.flatMap { code =>
      if (code.attribute("src").exists(_.startsWith("#"))) Map.empty[String,Doc] else {
        val id = code.attribute("id").getOrElse {
          code.id = nextId
          code.id
        }
        val md = mode(code)
        val doc = CodeMirror.Doc(stripIndentation(code.textContent), md.mime)
        val subdocs = subdocuments(doc,md.regex)
        attachDocument(id,doc,md)
        Map(id -> doc) ++ subdocs
      }
    }.toMap
  }

  def initializeEditors(root: NodeSeqQuery, documents: Map[String,Doc]) = {
    root.query("section code").elements.collect {
      case code if (!code.classes.contains("hidden")) =>
        val doc = code.attribute("id").flatMap(documents.get).orElse(
        code.attribute("src").collect {
          case src if src.startsWith("#") =>
            src.tail
        }.flatMap(documents.get)).getOrElse(CodeMirror.Doc(code.textContent, mode(code).mime : String))
        code.innerHTML = ""
        val editor = CodeMirror(code)
        editor.swapDoc(doc)
        editor.setOption("state-fragments",if (code.classes.contains("state-fragments"))
          (if (code.classes.contains("current-only")) "single" else "all") else null)
        editor.setOption("addModeClass",true)
        //editor.setOption("scrollbarStyle","null")
        editor.setOption("viewportMargin",js.eval("Infinity"))
        CobraJS.cmTheme.react(editor.setOption("theme",_))
        if (CobraJS.printing) editor.setOption("readOnly","nocursor")
        val handler: js.Function2[CodeMirror,raw.Event,Unit] = (instance: CodeMirror, event: raw.Event) => {
          val changes = event.asInstanceOf[js.Array[js.Dynamic]]
          if (changes.exists { change =>
            change.from.line.asInstanceOf[Int] != change.to.line.asInstanceOf[Int] ||
              change.text.asInstanceOf[js.Array[String]].length > 1
          }) {
            Reveal.sync()
          }
        }
        editor.on("changes", handler)
        editor
    }
  }
}
