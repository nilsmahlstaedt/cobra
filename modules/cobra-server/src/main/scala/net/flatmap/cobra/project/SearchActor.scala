package net.flatmap.cobra.project

import akka.actor.{Actor, ActorLogging, ActorRef, ReceiveTimeout}
import net.flatmap.cobra.{AmbiguousDefinition, Mode, ResolvedSnippet, ServerMessage, Snippet, SnippetDef, SnippetResolver, UnkownSnippet}
import net.flatmap.cobra.paths.Path
import net.flatmap.cobra.project.SearchActor.SnippetResponse

import scala.concurrent.duration._
import scala.util.Properties

/**
 * one off actor performing a search request encapsulated in a path
 */
class SearchActor(projects: Map[String, ActorRef], replyTo: ActorRef, reqId: String,  path: Path) extends Actor with ActorLogging {

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

  def collecting(waitingFor: Set[ActorRef], answers: List[SnippetResponse]): Receive = {
    case msg: SnippetResponse =>
      val nextWaitingFor = waitingFor - sender()
      val nextAnswers = msg::answers

      if(nextWaitingFor.isEmpty){
        replyTo ! produceResponse(nextAnswers, 0)
        context.stop(self)
      }else{
        context.become(collecting(nextWaitingFor, nextAnswers))
      }

    case ReceiveTimeout =>
      replyTo ! produceResponse(answers, waitingFor.size)
      log.info(s"search actor for $path ran into timeout while awaiting responses from the following actors: $waitingFor")
      context.stop(self)
  }

  def receive = {
    case ReceiveTimeout => context.stop(self)
  }

  def produceResponse(resps: List[SnippetResponse], missingAnswers: Int): ServerMessage = {
    val results: List[(String, Mode, String, Snippet)] = for{
      resp <- resps
      project = resp.project
      mode = resp.mode
      s <- resp.snippets
      kindString = s.kind.toString

    }yield (project, mode, kindString, s)

    results match {
      case Nil => UnkownSnippet(reqId, s"Could not find snippet for logical path $path")
      case (_, mode, _ , snippet)::Nil => ResolvedSnippet(
        reqId,
        SnippetResolver
          .getSourceLines(snippet)
          .mkString(Properties.lineSeparator),
        Some(mode)
      )
      case xs => AmbiguousDefinition(reqId, xs.map{
        case (proj, mode, kind, snippet) => SnippetDef(proj, kind, Path.buildPathString(snippet), snippet.source.toString, snippet.startLine, snippet.endLine)
      })
    }
  }
}
 object SearchActor {
   case class RequestSnippets(path: Path)
   case class SnippetResponse(project: String, mode: Mode, snippets: List[Snippet])
 }