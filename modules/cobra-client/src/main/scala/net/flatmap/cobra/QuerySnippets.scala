package net.flatmap.cobra

import java.util.UUID

import net.flatmap.js.util.NodeSeqQuery
import org.scalajs.dom.{Element, console, raw}
import scala.collection.mutable
import scala.concurrent.Future

object QuerySnippets {

  private type Inserter = (String, String => Unit)

  val insertionHandlers = mutable.Map.empty[String, String => Unit]

  def queryPathSnippets(root: NodeSeqQuery) = {
    root.query("div.sourceCode[src^='[']").elements.foreach { code => {
      val id = UUID.randomUUID().toString
      // request snippet from server
      val path = code.getAttribute("src")
      val codeElem = code.querySelector("code.sourceCode")
      if (codeElem != null) {
        (id, (snippet: String) => codeElem.innerText = snippet)
        requestSnippet(id, LogicalPath(path), codeElem)
      } else {
        throw new NoSuchElementException(s"could not find child 'code' element for element $code")
      }
    }
    }
  }

  def queryRemoteSrcSnippets(root: NodeSeqQuery)= {
    root.query(s"code[src]:not([src^='#'])").elements.foreach { code =>
      /*
        select all <code> nodes that posses a src attribute, which may not start with a '#'
       */
      val id = UUID.randomUUID().toString
      val src = PathSource(code.getAttribute("src"))
      requestSnippet(id, src, code)
    }
  }

  private def requestSnippet(id: String, source: SnippetSource, elem: Element) = {
    insertionHandlers.update(id, (content: String) => elem.innerText = content)
    CobraJS.send(GetSnippet(id, source))
  }

}
