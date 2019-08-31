package net.flatmap.cobra.project

import java.net.URLDecoder
import java.nio.charset.Charset

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import better.files._
import fastparse._
import net.flatmap.cobra._

import scala.util.{Failure, Success, Try}

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
      val t: Either[String, (ActorRef, GetSnippet)] = ProjectMaster.extractPathParts(path).flatMap{
        case (projectkey,snippetPath) =>
          projects.get(projectkey)
            .orElse(projects.get(projectkey.toLowerCase())) match {
            case None => Left(s"could not find registred project for key: '$projectkey'")
            case Some(ref) =>
              log.debug(s"found projectactor $ref for key $projectkey")
              Right((ref, GetSnippet(reqId, LogicalPath(snippetPath))))
          }
      }

      t.fold(
        err => sender() ! UnkownSnippet(reqId, err),
        ((a: ActorRef, msg: Any) => a.forward(msg)).tupled
      )

    case e => log.warning(s"received unkown message: $e")
  }
}

object ProjectMaster {
  def props(mainPID: Long, baseDir: File): Props = Props(new ProjectMaster(mainPID, baseDir))

  private def projectParser[_: P]: P[(String, String)] = {
    import SingleLineWhitespace._

    def projectKey: P[String] = P("[" ~ CharsWhile(_.isLetterOrDigit).! ~ "]")
    def snippetPath: P[String] = P(CharsWhile(!_.isWhitespace).!)

    P(Start ~ projectKey ~ snippetPath ~ End)
  }

  def extractPathParts(path: String): Either[String, (String, String)] = {
//    Try(URLDecoder.decode(path, Charset.forName("utf-8").name()))
//      .toEither
//      .left.map(_ => s"could not URL decode input '$path'")
//      .flatMap{ decoded =>
        parse(path, projectParser(_)) match {
          case Parsed.Success(value, _) => Right(value)
          case Parsed.Failure(label, index, extra) => Left(s"could not parse path $label at col $index")
        }
      //}
  }
}
