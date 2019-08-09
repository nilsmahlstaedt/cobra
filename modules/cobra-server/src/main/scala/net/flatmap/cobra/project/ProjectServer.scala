package net.flatmap.cobra.project

import java.nio.file.{Path, Paths}

import akka.actor.{Actor, ActorLogging, Props}
import net.flatmap.cobra.languageserver.{LSInteraction, LSLauncher}
import net.flatmap.cobra.util.FileUtils
import net.flatmap.cobra._

import scala.util.{Properties, Success, Try}

class ProjectServer(id: String, pid: Long, language: Language, rootPath: Path, srcRoots: List[Path]) extends Actor with ActorLogging {


  override def preStart(): Unit = {
    super.preStart()

    log.debug(s"Project Server $id initializing!")

    (for {
      ls <- LSLauncher.launch(language, rootPath, Some(pid))
    } yield {
      val projectFiles = FileUtils.findProjectFiles(language, rootPath::srcRoots).getOrElse(Nil)
      val snippets = LSInteraction.analyzeProjectFiles(ls, projectFiles)

      ls.exit()

      log.info(s"ProjectServer for project $id is initialized")
      context.become(running(snippets.map(s => (s"${s.parent.getOrElse("")}.${s.name}",s)).toMap))
    }) recover {
      case e: Throwable =>
        log.error(e, s"could not start project actor for project '$id'")
        context.stop(self)
    }

    log.debug(s"Project Server $id initialized")
  }

  override def receive: Receive = {
    // this function should not be called!
      case e => log.warning(s"received message before initializing! msg:$e")
  }

  def running(snippets: Map[String, Snippet]): Receive = {
    case InitProject(`id`, _, _, _) => sender() ! ProjectInitialized(id)
    case GetSnippet(reqId, LogicalPath(path)) => //TODO parse path
      snippets.get(path)
        .fold(sender() ! UnkownSnippet(reqId))(snippet => {
          sender() ! ResolvedSnippet(
            reqId,
            SnippetResolver
              .getSourceLines(snippet)
              .mkString(Properties.lineSeparator))
        })
    case e => log.error(s"received unknown message: $e")
  }
}

object ProjectServer {
  def props(pid: Long, id: String, mode:Mode, root: String, srcRoots:List[String] = Nil): Try[Props]  = {
    for{
      language <- Try{Languages.fromString(mode.name).get}
      rootPath <- Try{Paths.get(root)}
      srcRootPaths = srcRoots.map(sr => Try{Paths.get(sr)}).collect{
        case Success(p) => p
      }
    } yield {
      Props(new ProjectServer(id, pid, language,rootPath, srcRootPaths))
    }
  }
}