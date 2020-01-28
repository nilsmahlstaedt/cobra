package net.flatmap.cobra.paths

import fastparse._
import org.eclipse.lsp4j.SymbolKind

import scala.util.Try

trait PathParser {

  /**
   * parses general key structure of '[' tagChar ':' content ']'
   * @return result of content parser
   */
  protected def key[V](tagChar: String, content: => P[V])(implicit ctxt: P[_]): P[V] = {
    import SingleLineWhitespace._
    P(("[" ~ tagChar ~ ":" ~ content ~ "]"))
  }

  /**
   * reduces a list of keys by removing duplicate keys in last write wins fashion
   */
  protected def distinctKeys(xs: List[PathDetail]): List[PathDetail] = {
    var keys = Map.empty[Class[_ <: PathDetail], PathDetail]
    for(key <- xs){
      if(!keys.contains(key.getClass)){
        keys = keys + (key.getClass -> key)
      }
    }

    keys.values.toList
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
  protected def typeBound[_: P]: P[TypeBound] = {
    P(TypeNames.namesParser)
      .map(n => Try(SymbolKind.valueOf(n)))
      .filter(_.isSuccess)
      .map(_.get)
      .map(TypeBound.apply)
  }

  protected def projectAssociation[_: P]: P[ProjectAssociation] = {
    P(CharsWhile(_.isLetterOrDigit).!)
      .map(ProjectAssociation.apply)
  }

  protected def unknownKey[_:P]: P[Either[Unit, PathDetail]] = {
    import SingleLineWhitespace._
    P("[" ~ CharsWhile(_ != ']', 0) ~ "]" ~ Pass(Left(())))
  }

  protected def projectKey[_:P]: P[PathDetail] = key("p", projectAssociation)

  protected def typeKey[_:P]: P[PathDetail] = key("t", typeBound)


  protected def keys[_:P]: P[List[PathDetail]] = {
    import SingleLineWhitespace._
    P(
      (
        projectKey.map(Right(_)) |
          typeKey.map(Right(_)) |
          unknownKey
        ).rep()
    ).map(seq => seq.toList.collect{
      case Right(value) => value
    }).map(distinctKeys)
  }

  protected def projectParser[_: P]: P[Path] = {
    import SingleLineWhitespace._

    def snippetPath: P[String] = pathElems.map(_.mkString(".")) // P(CharsWhile(!_.isWhitespace).!)

    P(Start ~ keys ~ snippetPath ~ End)
      .map {
        case (keys, path) => Path(path, keys)
      }
  }

  protected def pathElems[_:P]: P[List[String]] = {
    import SingleLineWhitespace._

    def elem: P[String] = CharsWhile(_.isLetterOrDigit).!
    def seperator: P[Unit] = CharIn(".",  "/", "#")
    // added "/" and "#" for normalizing of strange LS outputs

    P(seperator.? ~ elem ~ (seperator ~ elem).rep)
      .map{
        case (first, rest) => first :: rest.toList
      }
  }

  /**
   * splits a path into it's elements
   * @return
   */
  def producePathElems(path: String): List[String] = {
    parse(path, pathElems(_)) match {
      case Parsed.Success(value, _) => value
      case Parsed.Failure(_,_,_) => Nil
    }
  }

  def extractPathParts(path: String): Either[String, Path] = {
    parse(path, projectParser(_)) match {
      case Parsed.Success(value, _) => Right(value)
      case f@Parsed.Failure(_, index, _) => Left(s"could not parse path at col $index. $f")
    }
  }
}

object PathParser extends PathParser
