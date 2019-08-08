package net.flatmap.cobra

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import better.files._

import scala.util.{Failure, Properties, Success, Try}

class ProjectMaster(mainPID: Long) extends Actor with ActorLogging {

  override def receive: Receive = running(Map.empty)

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

    case GetSnippet(reqId, PathSource(path, startOpt, endOpt)) =>
      getPathSnippet(path, startOpt, endOpt) match {
        case Success(content) =>
          log.debug(s"resolved snippet $reqId")
          sender() ! ResolvedSnippet(reqId, content)
        case Failure(ex) =>
          log.info(s"could not resolve snippet $reqId", ex)
          sender() ! UnkownSnippet(reqId)
      }

    case GetSnippet(reqId, SubsnippetSource(base, part, mode)) => // extract sub-snippet from base and answer
    case msg@GetSnippet(reqId, LogicalPath(path)) => // parse path and forward message to project actor
    case e => log.warning(s"received unkown message: $e")
  }

  def getPathSnippet(path: String, from: Option[Int], to: Option[Int]): Try[String] = {
    for {
      lines <- Try {
        File(path).lines
      }
      firstLine = from.getOrElse(0)
      lastLines = to.getOrElse(lines.size)
    } yield {
      lines
        .slice(firstLine, lastLines - firstLine)
        .mkString(Properties.lineSeparator)
    }
  }

}

object ProjectMaster {
  def props(mainPID: Long): Props = Props(new ProjectMaster(mainPID))
}
