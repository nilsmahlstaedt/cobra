package net.flatmap.cobra.project

import akka.actor.{Actor, ActorLogging, ActorRef, ReceiveTimeout}
import net.flatmap.cobra.{AmbiguousDefinition, Mode, ResolvedSnippet, ServerMessage, Snippet, SnippetDef, SnippetResolver, UnkownSnippet}
import net.flatmap.cobra.paths.Path
import net.flatmap.cobra.project.SearchActor.{RequestSnippets, SnippetResponse}

import scala.concurrent.duration._
import scala.util.Properties

/**
 * one off actor performing a search request encapsulated in a path
 */
class SearchActor(projects: Map[String, ActorRef], replyTo: ActorRef, reqId: String,  path: Path) extends Actor with ActorLogging {

  override def preStart(): Unit = {
    super.preStart()
    // stop self if not receiving an answer for a second
    context.setReceiveTimeout(1.second)

    val toSearch = projectsToSearch(projects, path)

    //query actors
    toSearch.foreach(_ ! RequestSnippets(path))

    // start collecting responses
    context.become(collecting(toSearch.toSet, Nil))
  }

  /**
   * returns the projectserver actors to search
   *
   * will try to return only the explicitly stated project if done,
   * otherwise returns all projects if none is specified
   *
   * @param ps projects maps (projectID -> server actor ref)
   * @param p path to search
   * @return actors to query for results
   */
  private def projectsToSearch(ps: Map[String, ActorRef], p: Path): List[ActorRef] = {
    p.projectAssociation()
      .fold(projects.values.toList) { pa =>
        ps.get(pa.project).toList
      }
  }

  /**
   * answer collecting state of actor
   * @param waitingFor queried refs who haven't answered yet
   * @param answers answers received
   */
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

  /**
   * default behaviour
   */
  def receive = {
    case ReceiveTimeout => context.stop(self)
  }

  /**
   * analyses collected responses and formulates response
   * formulated response answers the search encapsulated in this actor
   * @param resps collected responses
   * @param missingAnswers number of missing answers
   * @return a message that
   */
  def produceResponse(resps: List[SnippetResponse], missingAnswers: Int): ServerMessage = {
    case class SearchResult(project: String, mode:Mode, kind: String, snippet: Snippet){
      def toSnippetDef: SnippetDef =
        SnippetDef(project, kind, Path.buildPathString(snippet), snippet.source.toString, snippet.startLine, snippet.endLine)
    }

    val results: List[SearchResult] = for{
      resp <- resps
      project = resp.project
      mode = resp.mode
      s <- resp.snippets
      kindString = s.kind.toString
    }yield SearchResult(project, mode, kindString, s)

    results match {
      case Nil => UnkownSnippet(reqId, s"Could not find snippet for logical path $path")
      case SearchResult(_, mode, _ , snippet)::Nil => ResolvedSnippet(
        reqId,
        SnippetResolver
          .getSourceLines(snippet)
          .mkString(Properties.lineSeparator),
        Some(mode)
      )
      case xs => AmbiguousDefinition(reqId, xs.map(_.toSnippetDef))
    }
  }
}

 object SearchActor {
   case class RequestSnippets(path: Path)
   case class SnippetResponse(project: String, mode: Mode, snippets: List[Snippet])
 }