package net.flatmap.cobra.project

import java.net.URLDecoder
import java.nio.charset.Charset

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import better.files._
import fastparse._
import net.flatmap.cobra._
import org.eclipse.lsp4j.SymbolKind

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

  sealed abstract class PathDetail
  case class ProjectAssociation(project: String) extends PathDetail
  case class TypeBound(typ: SymbolKind) extends PathDetail

  case class Path(path: String, details: List[PathDetail]){
    def isAbsolute: Boolean = path.startsWith("..")
  }


  /**
   * parses a typename as defined in [[TypeNames.names]] in case sensitive form
   *
   * should the input not match any of the names directly, it is again parsed case insensitive
   * in this case all words from above, that contain more than just a capitalized first character are excluded
   * after that the input is lower cased and then the first letter is capitalized
   *
   * the resulting string from both parsers is converted into the Enum Value ([[SymbolKind]]) that it represents
   * in a safe manner.
   */
  private def typeBound[_:P]: P[TypeBound] = {
    def nameCS = StringIn(TypeNames.names:_*).!

    def fixableNames = StringInIgnoreCase(TypeNames.singleWords:_*).!.map(_.toLowerCase.capitalize)

    def name: P[String] = nameCS | fixableNames

    P(name)
      .map(n => Try(SymbolKind.valueOf(n)))
      .filter(_.isSuccess)
      .map(_.get)
      .map(TypeBound.apply)
  }

  private def projectAssociation[_:P]: P[ProjectAssociation] = {
    P(CharsWhile(_.isLetterOrDigit).!)
      .map(ProjectAssociation.apply)
  }

  private def projectParser[_: P]: P[Path] = {
    import SingleLineWhitespace._

    def key[V](content: P[V]): P[Option[V]] = P("[" ~ content  ~ "]").?

    def projectKey: P[Option[PathDetail]] = key(projectAssociation)
    def typeKey: P[Option[PathDetail]] = key(typeBound)

    def snippetPath: P[String] = P(CharsWhile(!_.isWhitespace).!)

    P(Start ~ projectKey ~ typeKey ~ snippetPath ~ End)
      .map{
        case (pk, tk, path) => Path(path, pk.toList ++ tk.toList)
      }
  }

  def extractPathParts(path: String): Either[String, Path] = {
        parse(path, projectParser(_)) match {
          case Parsed.Success(value, _) => Right(value)
          case Parsed.Failure(label, index, extra) => Left(s"could not parse path $label at col $index")
        }
      //}
  }
}
