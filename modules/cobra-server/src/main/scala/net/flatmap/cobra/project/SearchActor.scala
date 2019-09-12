package net.flatmap.cobra.project

import akka.actor.{Actor, ActorLogging, ActorRef, ReceiveTimeout}
import net.flatmap.cobra.paths.Path

import scala.concurrent.duration._

/**
 * one off actor performing a search request encapsulated in a path
 */
class SearchActor(projects: Map[String, ActorRef], replyTo: ActorRef, path: Path) extends Actor with ActorLogging {


  override def preStart(): Unit = {
    super.preStart()
    context.setReceiveTimeout(1.second)
  }

  def projectsToSearch(ps: Map[String, ActorRef], p: Path): List[ActorRef] = {
    p.projectAssociation()
      .fold(projects.values.toList) { pa =>
        ps.get(pa.project).toList
      }
  }

  def collecting(waitingFor: Set[ActorRef], answers: List[SnippetR])

  def receive = {
    case ReceiveTimeout => context.stop(self)
  }
}
