package net.flatmap.cobra.project

import java.nio.file.Path

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props, Terminated}
import better.files._
import net.flatmap.cobra._
import net.flatmap.cobra.paths.PathParser

import scala.util.{Failure, Success}

class ProjectMaster(mainPID: Long, baseDir: File) extends Actor with ActorLogging {

  override def receive: Receive = running(Map.empty)

  private val baseDirStr = baseDir.path.toAbsolutePath.toString

  def running(projects: Map[String, ActorRef]): Receive = {
    case ResetAllSnippets =>
      projects.foreach{
        case (_, ref) => ref ! PoisonPill
      }
    case msg@InitProject(id, _, _, _) =>
      var updatedProjects = projects

      if(projects.contains(id)){
        log.debug(s"removing project actor for $id")
        context.stop(projects(id))
        updatedProjects = updatedProjects - id
      }

      log.debug(s"starting project actor for $id")
      initProject(msg).foreach{
        case (id, projectServer) =>
          projectServer.forward(msg)
          context.become(running(updatedProjects + (id -> projectServer)))
      }

    case msg: InitProject =>
      //println(projects)
      initProject(msg).foreach{
        case (id, projectServer) => context.become(running(projects + (id -> projectServer)))
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

  def initProject(initMsg: InitProject): Option[(String,ActorRef)] = {
    ProjectServer
      .props(mainPID, initMsg.id, initMsg.mode, initMsg.root, initMsg.srcRoots)
      .map(context.actorOf(_, initMsg.id)) match {
      case Failure(exception) =>
        log.error(s"could not initialize project ${initMsg.id}", exception)
        None
      case Success(pRef) =>
        log.info(s"started project actor for ${initMsg.id}")
        context.watch(pRef)
        pRef.forward(initMsg)
        Some((initMsg.id, pRef))
    }
  }
}

object ProjectMaster {
  def props(mainPID: Long, baseDir: File): Props = Props(new ProjectMaster(mainPID, baseDir))

}
