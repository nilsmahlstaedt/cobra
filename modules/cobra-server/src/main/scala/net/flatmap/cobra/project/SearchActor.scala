package net.flatmap.cobra.project

import akka.actor.{Actor, ActorLogging, ActorRef, Props, ReceiveTimeout}
import net.flatmap.cobra.paths.{Path, PathParser}
import net.flatmap.cobra.project.SearchActor.{RequestSnippets, SearchRequest, SnippetResponse}
import net.flatmap.cobra._

import scala.concurrent.duration._
import scala.util.Properties

/**
 * one off actor performing a search request encapsulated in a path
 */
class SearchActor(reqId: String, reqPath: String, projects: Map[String, ActorRef], replyTo: ActorRef) extends Actor with ActorLogging {

  override def preStart(): Unit = {
    super.preStart()

    PathParser.extractPathParts(reqPath) match {
      case Left(error) =>
        sender() ! UnknownSnippet(reqId, error)
        context.stop(self)
      case Right(logicalPath) =>
        // initiate search
        self ! SearchActor.SearchRequest(logicalPath)
        // stop self if not receiving an answer for seconds to prevent resource leaks
        // and to guarantee a response to the client
        context.setReceiveTimeout(30.seconds)
    }
  }

  /**
   * returns the projectserver actors to search
   *
   * will try to return only the explicitly stated project if done,
   * otherwise returns all projects if none is specified
   *
   * @param ps projects maps (projectID -> server actor ref)
   * @param p  path to search
   * @return actors to query for results
   */
  private def projectsToSearch(ps: Map[String, ActorRef], p: Path): List[ActorRef] = {
    p.projectAssociation()
      .fold(ps.values.toList) { pa =>
        ps.get(pa.project).toList
      }
  }

  /**
   * answer collecting state of actor
   *
   * @param waitingFor queried refs who haven't answered yet
   * @param answers    answers received
   *
   */
  def collecting(waitingFor: Set[ActorRef], answers: List[SnippetResponse], request: SearchRequest): Receive = {
    case msg: SnippetResponse =>
      val nextWaitingFor = waitingFor - sender()
      val nextAnswers = msg :: answers

      if (nextWaitingFor.isEmpty) {
        replyTo ! produceResponse(nextAnswers, 0, request)
        context.stop(self)
      } else {
        context.become(collecting(nextWaitingFor, nextAnswers, request))
      }

    case ReceiveTimeout =>
      replyTo ! produceResponse(answers, waitingFor.size, request)
      log.info(s"search actor for ${request.path} ran into timeout while awaiting responses from the following actors: $waitingFor")
      context.stop(self)
  }

  /**
   * default behaviour
   */
  def receive: Receive = {
    case _:SearchActor.SearchRequest if projects.isEmpty =>
      replyTo ! UnknownSnippet(reqId, s"search initiated for a set of 0 projects!")
      context.stop(self)

    case req@SearchActor.SearchRequest(path) =>
      // start searching
      val toSearch = projectsToSearch(projects, path)
      if(toSearch.isEmpty){
        // no project found
        replyTo ! UnknownSnippet(reqId, s"no project found for project id '${path.projectAssociation().map(_.project).getOrElse("")}'")
        context.stop(self)
      }else{
        //query project actors
        toSearch.foreach(_ ! RequestSnippets(path))
        // start collecting responses
        context.become(collecting(toSearch.toSet, Nil, req))
      }

    case ReceiveTimeout =>
      // just die
      context.stop(self)
  }

  /**
   * analyses collected responses and formulates response
   * formulated response answers the search encapsulated in this actor
   *
   * @param resps          collected responses
   * @param missingAnswers number of missing answers
   * @param request        search request to respond to
   * @return a message that
   */
  def produceResponse(resps: List[SnippetResponse], missingAnswers: Int, request: SearchRequest): ServerMessage = {
    case class SearchResult(project: String, mode: Mode, kind: String, snippet: Snippet) {
      def toSnippetDef: SnippetDef =
        SnippetDef(project, kind, Path.buildPathString(snippet), snippet.source.toString, snippet.startLine, snippet.endLine)
    }

    val results: List[SearchResult] = for {
      resp <- resps
      project = resp.project
      mode = resp.mode
      s <- resp.snippets
      kindString = s.kind.toString
    } yield SearchResult(project, mode, kindString, s)

    results match {
      case Nil => UnknownSnippet(reqId, s"Could not find snippet for logical path ${request.path}")
      case SearchResult(_, mode, _, snippet) :: Nil => ResolvedSnippet(
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

  case class SearchRequest(path: Path)

  def props(reqID: String, reqPath: String, projects: Map[String, ActorRef], replyTo: ActorRef): Props =
    Props(new SearchActor(reqID, reqPath, projects, replyTo))
}