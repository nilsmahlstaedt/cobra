package net.flatmap.cobra.project

import akka.actor.{Actor, ActorLogging}
import net.flatmap.cobra._
import better.files._
import scala.util.{Failure, Properties, Success, Try}

/**
  * one of actor fetching a snippet from a file path
  * @param basePath base dir path from which to walk the supplied path from
  */
class SourceLoadActor(basePath: String) extends Actor with ActorLogging {

  override def receive: Receive = {
    case GetSnippet(reqId, PathSource(path, from, to)) =>
      (for {
        lines <- Try {
          (basePath / path).lines
        }
        firstLine = from.getOrElse(0)
        lastLines = to.getOrElse(lines.size)
      } yield {
        lines
          .slice(firstLine, lastLines - firstLine)
          .mkString(Properties.lineSeparator)
      }) match {
        case Success(content) =>
          log.debug(s"resolved snippet $reqId")
          sender() ! ResolvedSnippet(reqId, content)
        case Failure(ex) =>
          log.info(s"could not resolve snippet $reqId")
          log.info(ex.toString)
          sender() ! UnkownSnippet(reqId)
      }

      context.stop(self)
  }
}
