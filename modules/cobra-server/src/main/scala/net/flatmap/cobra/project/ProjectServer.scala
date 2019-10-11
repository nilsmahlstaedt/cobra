package net.flatmap.cobra.project

import java.nio.file.{Path, Paths, StandardWatchEventKinds, WatchService}

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import net.flatmap.cobra._
import net.flatmap.cobra.languageserver.{LSInteraction, LSLauncher}
import net.flatmap.cobra.project.SearchActor.{RequestSnippets, SnippetResponse}
import net.flatmap.cobra.util.FileUtils
import org.eclipse.lsp4j.services.LanguageServer
import better.files._
import FileWatcher._
import net.flatmap.cobra.paths.SnippetSearch
import net.flatmap.cobra.project.ProjectServer.ProjectFileUpdate

import scala.util.{Success, Try}

class ProjectServer(projectId: String, pid: Long, language: Language, mode:Mode, rootPath: Path, srcRoots: List[Path]) extends Actor with ActorLogging with SnippetSearch {

  implicit class IfEmptyList[A](l: List[A]){
    def orIfEmpty(other: A): List[A] = orIfEmpty(List(other))
    def orIfEmpty(other: List[A]): List[A] = if(l.isEmpty){
      other
    }else{
      l
    }
  }

  /**
   * holds language server to be exited when actor dies
   */
  private var languageServerPlaceholder: Option[LanguageServer] = None

  override def preStart(): Unit = {
    super.preStart()

    implicit val system: ActorSystem = context.system
    log.debug(s"Project Server $projectId initializing!")

    log.debug("start language server")
    (for {
      ls: LanguageServer <- LSLauncher.launch(language, rootPath, Some(pid))
    } yield {
      languageServerPlaceholder = Some(ls)

      log.debug("finding project files")
      val projectFiles = FileUtils.findProjectFiles(language, srcRoots.orIfEmpty(rootPath)).getOrElse(Nil).distinct
      log.debug("analyzing project files")
      val snippets = LSInteraction.analyzeProjectFiles(ls, projectFiles)

      log.debug("creating file watches")
      snippets.flatMap(s => Option(s.source.normalize().getParent).map((_, s.source)).toList).foreach((watchFile _).tupled)

      //ls.exit()

      log.info(s"ProjectServer for project $projectId is initialized")

      /*
      the snippets will be searched infrequently at a time, where the user is waiting anyways, so there is no need to
      make the search itself super efficient. The addition overhead of constructing maps over maps of snippet
      will probably outweigh the search time penalty
       */
      
//      val snippetmap: Map[String, Snippet] = snippets.map(s => {
//        val parent = s.parent.getOrElse("").replaceAll("/", ".")
//        val name = s.name
//
//        (s"$parent$name", s)
//      }).foldLeft(Map.empty[String, Snippet])({
//        case (acc,(p, s)) =>
//          if (acc.get(p).nonEmpty) println(s"overwriting: $p (${acc(p).kind.getValue} -> ${s.kind.getValue})")
//          acc + (p -> s)
//      })

      context.become(running(ls, snippets))
    }) recover {
      case e: Throwable =>
        log.error(e, s"could not start project actor for project '$projectId'")
        context.stop(self)
    }

    log.debug(s"Project Server $projectId initialized")
  }

  override def postStop(): Unit = {
    super.postStop()
    languageServerPlaceholder.foreach(_.exit())
  }

  /**
   * creates filewatch for snippetPath in directory dir
   *
   * sends watch events to self
   *
   * @param dir directory path to watch for changes
   * @param snippetPath path to watch out for in dir
   */
  private def watchFile(dir: Path, snippetPath: Path) (implicit sys: ActorSystem): Unit = {
    context.actorOf(File(dir).watcherProps(recursive = false)) ! when(
      StandardWatchEventKinds.ENTRY_CREATE,
      StandardWatchEventKinds.ENTRY_DELETE,
      StandardWatchEventKinds.ENTRY_MODIFY) {
      case (event, file) if file.path.equals(snippetPath) =>
        log.debug(s"${context.self.path.name} got file update for existing file $file")
        self ! ProjectFileUpdate(event, file)
      case (event, file) if mode.fileendings.contains(file.extension.getOrElse("")) =>
        log.debug(s"${context.self.path.name} got file update for new file $file")
        self ! ProjectFileUpdate(event, file)
      case (event, file) =>
        log.debug(s"IGNORED - file update $event for $file")
    }
  }

  override def receive: Receive = {
      case e => log.warning(s"received message before initializing! msg:$e")
  }

  def running(ls: LanguageServer, dict: List[Snippet]): Receive = {
    case InitProject(`projectId`, _, _, _) => sender() ! ProjectInitialized(projectId)
    case RequestSnippets(path) => sender() ! SnippetResponse(projectId, mode, dict.findSnippets(path))

      // update dict when project files change
    case ProjectFileUpdate(StandardWatchEventKinds.ENTRY_CREATE, file) =>
      LSInteraction.analyzeFile(ls)(file.path).foreach(newSnippets => context.become(running(ls, dict ++ newSnippets)))
    case ProjectFileUpdate(StandardWatchEventKinds.ENTRY_DELETE, file) =>
      val pathToRemove = file.path
      context.become(running(ls, dict.filterNot(_.source.equals(pathToRemove))))
    case ProjectFileUpdate(StandardWatchEventKinds.ENTRY_MODIFY, file) =>
      val pathOfUpdate = file.path
      val oldRemoved = dict.filterNot(_.source.equals(pathOfUpdate))
      val newSnippets = LSInteraction.analyzeFile(ls)(pathOfUpdate).toOption.toList.flatten
      context.become(running(ls, oldRemoved ++ newSnippets))

    case e => log.error(s"received unknown message: $e")
  }

//  def running(snippets: Map[String, Snippet]): Receive = {
//    case InitProject(`projectId`, _, _, _) => sender() ! ProjectInitialized(projectId)
//    case GetSnippet(reqId, LogicalPath(path)) => //path is already adjusted by project master!
//
//      log.info(snippets.toList.map{
//        case (p, s) => s"$p (${s.endLine-s.startLine+1}) (${s.startLine+1}-${s.endLine+1})" //keys.toList.sorted.mkString("\n")
//      }.sorted.mkString("\n"))
//
//      snippets.get(path)
//        .fold(sender() ! UnkownSnippet(reqId, "path not found"))(snippet => {
//          sender() ! ResolvedSnippet(
//            reqId,
//            SnippetResolver
//              .getSourceLines(snippet)
//              .mkString(Properties.lineSeparator),
//            Some(mode)
//          )
//        })
//    case e => log.error(s"received unknown message: $e")
//  }
}

object ProjectServer {

  case class ProjectFileUpdate(event: Event, file: File)

  /**
   * generate props for a new project server
   * @param pid cobra main thread pid (watched by LS for auto exit if dead)
   * @param id project id
   * @param mode language
   * @param root project root dir
   * @param srcRoots src dir roots
   *                 (will be resolved relative to project root if given in relative form)
   * @return
   */
  def props(pid: Long, id: String, mode:Mode, root: String, srcRoots:List[String] = Nil): Try[Props]  = {
    for{
      language <- Try{Languages.fromString(mode.name).get}
      rootPath <- Try{Paths.get(root).toAbsolutePath}
      srcRootPaths = srcRoots.map(sr => Try{
       Paths.get(sr) match {
         case p if p.isAbsolute => p
         case p => rootPath.resolve(p)
       }
      }).collect{
        case Success(p) => p
      }
    } yield {
      Props(new ProjectServer(id, pid, language, mode, rootPath, srcRootPaths))
    }
  }
}