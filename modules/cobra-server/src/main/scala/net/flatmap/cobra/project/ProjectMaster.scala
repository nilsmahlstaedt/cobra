package net.flatmap.cobra.project

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import better.files._
import fastparse._
import net.flatmap.cobra._

import scala.util.{Failure, Success}

class ProjectMaster(mainPID: Long, baseDir: File) extends Actor with ActorLogging {

  override def receive: Receive = running(Map.empty)

  private val baseDirStr = baseDir.path.toAbsolutePath.toString

  def running(projects: Map[String, ActorRef]): Receive = {
    case msg@InitProject(id, _, _, _) if projects.contains(id) =>
      projects(id).tell(msg, sender)

    case msg@InitProject(id, mode, root, srcs) =>
      ProjectServer
        .props(mainPID, id, mode, root, srcs)
        .map(context.actorOf) match {
        case Failure(exception) =>
          log.error(s"could not initialize project $id", exception)
        case Success(pRef) =>
          log.info(s"started project actor for $id")
          context.watch(pRef)
          pRef.forward(msg)
          context.become(running(projects + (id -> pRef)))
      }

    case Terminated(ref) =>
      projects.find { case (key, value) => value.equals(ref) }.foreach {
        case (deadKey, deadRef) =>
          log.warning(s"project actor for '$deadKey' died")
          context.become(running(projects - deadKey))
      }

    case msg@GetSnippet(_, _: PathSource) =>
      context.actorOf(Props(new SourceLoadActor(baseDirStr))).forward(msg)
    case msg@GetSnippet(reqId, LogicalPath(path)) =>
      (for{
        (projectkey, snippetPath) <- extractPathParts(path)
        actor <- projects.get(projectkey).orElse(projects.get(projectkey.toLowerCase()))
      } yield {
        (actor, GetSnippet(reqId, LogicalPath(snippetPath)))
      }).fold(sender() ! UnkownSnippet(reqId)){
        case (actor, msg) => actor.forward(msg)
      }

    case e => log.warning(s"received unkown message: $e")
  }

  private def projectParser[_: P]: P[(String, String)] = {
    import NoWhitespace._

    val projectKey = P("[" ~ CharsWhileIn(" a-zA-Z0-9").?.! ~ "]")
    val snippetPath = P(CharsWhile(!_.isWhitespace).!)

    P(Start ~ projectKey ~/ " ".rep(1) ~/ snippetPath ~ End)
  }

  private def extractPathParts(path: String): Option[(String, String)] = {
    parse(path, projectParser(_)) match {
      case Parsed.Success(value, index) => Some(value)
      case _: Parsed.Failure => None
    }
  }
}

object ProjectMaster {
  def props(mainPID: Long, baseDir: File): Props = Props(new ProjectMaster(mainPID, baseDir))
}
