package net.flatmap.cobra.project

import java.nio.file.{Path, Paths}

import akka.actor.{Actor, ActorLogging, Props}
import net.flatmap.cobra.languageserver.{LSInteraction, LSLauncher}
import net.flatmap.cobra.util.FileUtils
import net.flatmap.cobra._
import net.flatmap.cobra.project.SearchActor.{RequestSnippets, SnippetResponse}

import scala.util.{Properties, Success, Try}

class ProjectServer(projectId: String, pid: Long, language: Language, mode:Mode, rootPath: Path, srcRoots: List[Path]) extends Actor with ActorLogging with SnippetSearch {

  implicit class IfEmptyList[A](l: List[A]){
    def orIfEmpty(other: A): List[A] = orIfEmpty(List(other))
    def orIfEmpty(other: List[A]): List[A] = if(l.isEmpty){
      other
    }else{
      l
    }
  }

  override def preStart(): Unit = {
    super.preStart()

    log.debug(s"Project Server $projectId initializing!")

    (for {
      ls <- LSLauncher.launch(language, rootPath, Some(pid))
    } yield {
      val projectFiles = FileUtils.findProjectFiles(language, srcRoots.orIfEmpty(rootPath)).getOrElse(Nil).distinct
      val snippets = LSInteraction.analyzeProjectFiles(ls, projectFiles)

      ls.exit()

      log.info(s"ProjectServer for project $projectId is initialized")
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

      context.become(running(snippets))
    }) recover {
      case e: Throwable =>
        log.error(e, s"could not start project actor for project '$projectId'")
        context.stop(self)
    }

    log.debug(s"Project Server $projectId initialized")
  }

  override def receive: Receive = {
      case e => log.warning(s"received message before initializing! msg:$e")
  }

  def running(dict: List[Snippet]): Receive = {
    case InitProject(`projectId`, _, _, _) => sender() ! ProjectInitialized(projectId)
    case RequestSnippets(path) => sender() ! SnippetResponse(projectId, mode, dict.findSnippets(path))
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