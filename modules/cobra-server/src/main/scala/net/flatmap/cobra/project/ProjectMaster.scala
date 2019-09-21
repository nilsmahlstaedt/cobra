package net.flatmap.cobra.project

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import better.files._
import net.flatmap.cobra._
import net.flatmap.cobra.paths.PathParser

import scala.util.{Failure, Success}

class ProjectMaster(mainPID: Long, baseDir: File) extends Actor with ActorLogging {

  override def receive: Receive = running(Map.empty)

  private val baseDirStr = baseDir.path.toAbsolutePath.toString

  def running(projects: Map[String, ActorRef]): Receive = {
    case msg@InitProject(id, _, _, _) if projects.contains(id) =>
      println(projects)

      projects(id).tell(msg, sender)

    case msg@InitProject(id, mode, root, srcs) =>
      println(projects)

      ProjectServer
        .props(mainPID, id, mode, root, srcs)
        .map(context.actorOf(_, id)) match {
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
    case GetSnippet(reqId, LogicalPath(path)) =>
      PathParser.extractPathParts(path) match {
        case Left(error) => sender() ! UnkownSnippet(reqId, error)
        case Right(logicalPath) =>
          // initiate search, task actor will respond to original sender
          context.actorOf(SearchActor.props()) ! SearchActor.SearchRequest(reqId, projects, logicalPath, sender())
      }

    case e => log.warning(s"received unkown message: $e")
  }
}

object ProjectMaster {
  def props(mainPID: Long, baseDir: File): Props = Props(new ProjectMaster(mainPID, baseDir))

}
