package net.flatmap.cobra

import java.nio.ByteBuffer

import boopickle.Default._
import net.flatmap.collaboration._

import scala.util.matching.Regex

sealed trait ClientMessage
sealed trait ServerMessage
sealed trait SnippetMessage { val id: String }
sealed trait ProjectMessage { val id: String }

case object HeartBeat extends ClientMessage with ServerMessage

case class WatchFile(path: String) extends ClientMessage
case class FileUpdate(path: String) extends ServerMessage

/** initialize project on the server side and build up dictionary */
case class InitProject(id: String, mode: Mode, root: String, srcRoots: List[String]) extends ClientMessage with ProjectMessage
case class ProjectInitialized(id: String) extends ServerMessage with ProjectMessage

case class GetSnippet(id: String, source: SnippetSource) extends ClientMessage with ProjectMessage
// TODO add a mode option to the snippet response
case class ResolvedSnippet(id: String, content: String) extends ServerMessage with ProjectMessage
case class UnkownSnippet(id: String, msg: String) extends ServerMessage with ProjectMessage

sealed trait SnippetSource
case class PathSource(path: String, startLine: Option[Int] = None, endLine: Option[Int] = None) extends SnippetSource
case class SubsnippetSource(base: String, partId: String, mode: Mode) extends SnippetSource
case class LogicalPath(path: String) extends SnippetSource

case class InitDoc(id: String, file: String, mode: Mode) extends ClientMessage with SnippetMessage
case class Edit(id: String, operation: Operation[Char], revision: Long) extends ClientMessage with SnippetMessage
case class Annotate(id: String, aid: String, annotations: Annotations, revision: Long) extends ClientMessage with SnippetMessage

case class Sendback(id: String, props: Map[String,String], s: String) extends ClientMessage with ServerMessage with SnippetMessage

case class RequestInfo(id: String, from: Int, to: Int, uid: String) extends ClientMessage with ServerMessage with SnippetMessage
case class Information(id: String, from: Int, to: Int, body: String, uid: String) extends ClientMessage with ServerMessage with SnippetMessage

case class AcknowledgeEdit(id: String) extends ServerMessage with SnippetMessage
case class RemoteEdit(id: String, op: Operation[Char]) extends ServerMessage with SnippetMessage
case class RemoteAnnotations(id: String, aid: String, annotations: Annotations) extends ServerMessage with SnippetMessage
case class CombinedRemoteEdit(id: String, op: Operation[Char], revisions: Long) extends ServerMessage with SnippetMessage
case class ResetSnippet(id: String, content: String, revision: Long) extends ServerMessage with SnippetMessage

case class RevealOptionsUpdate(values: Map[String,String]) extends ServerMessage
case class TitleUpdate(newTitle: String) extends ServerMessage
case class ThemeUpdate(code: String, slides: String) extends ServerMessage
case class LanguageUpdate(newLang: String) extends ServerMessage
case class SnippetChanged(src: String) extends ServerMessage

// TODO: This is a temporary hack until snippet management is implemented on the server.
case object ResetAllSnippets extends ClientMessage

trait Picklers {
  implicit val SnippetSourcePickler: Pickler[SnippetSource] =
    compositePickler[SnippetSource]
    .addConcreteType[PathSource]
    .addConcreteType[SubsnippetSource]
    .addConcreteType[LogicalPath]

  implicit val charActionPickler: Pickler[Action[Char]] =
    compositePickler[Action[Char]]
      .addConcreteType[Retain]
      .addConcreteType[Insert[Char]]
      .addConcreteType[Delete]

  implicit val annotationPickler: Pickler[Annotation] =
    compositePickler[Annotation]
      .addConcreteType[Empty]
      .addConcreteType[Annotated]

  implicit val modePickler: Pickler[Mode] =
    compositePickler[Mode]
      .addConcreteType[Scala.type]
      .addConcreteType[Isabelle.type]
      .addConcreteType[Plain.type]
      .addConcreteType[Haskell.type]

  implicit val annotationMessagePickler: Pickler[AnnotationMessage] =
    compositePickler[AnnotationMessage]
      .addConcreteType[ErrorMessage]
      .addConcreteType[WarningMessage]
      .addConcreteType[InfoMessage]
      .addConcreteType[OutputMessage]
      .addConcreteType[StateMessage]
}

object ClientMessage extends Picklers {
  def read(bytes: ByteBuffer): ClientMessage = Unpickle[ClientMessage].fromBytes(bytes)
  def write(message: ClientMessage): ByteBuffer = Pickle.intoBytes(message)
}

object ServerMessage extends Picklers {
  def read(bytes: ByteBuffer): ServerMessage = Unpickle[ServerMessage].fromBytes(bytes)
  def write(message: ServerMessage): ByteBuffer = Pickle.intoBytes(message)
}