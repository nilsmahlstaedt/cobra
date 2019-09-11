package net.flatmap.cobra.paths

import fastparse._
import org.eclipse.lsp4j.SymbolKind

import scala.util.Try

trait PathParser {
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
    //import NoWhitespace._
    //def nameCS = StringIn(TypeNames.names:_*).!

    //def fixableNames = StringInIgnoreCase(TypeNames.singleWords:_*).!.map(_.toLowerCase.capitalize)

    //def name: P[String] = P(StringInIgnoreCase(TypeNames.names: _*).!).map(s => TypeNames.recapitalize(s.toLowerCase))

//    def names = {
//      val parsers = TypeNames.names.map(s => P(s.!))
//      var combined = parsers.head
//
//      for (p <- parsers.tail) {
//        combined = P(combined | (p))
//      }
//      combined.map(_.toLowerCase).map(TypeNames.recapitalize)
//    }

    //def names2 = TypeNames.names
    //  .map(s => P(s.!))
    //  .foldLeft(P(Fail).map((_: Nothing) => ""))((acc, p) => P(acc | p))
    //  .map(_.toLowerCase)
    //  .map(TypeNames.recapitalize)

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

  protected def key[V](tagChar: String, content: => P[V])(implicit ctxt: P[_]): P[V] = {
    import SingleLineWhitespace._
    P(("[" ~ tagChar ~ ":" ~ content ~ "]"))
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

  protected def distinctKeys(xs: List[PathDetail]): List[PathDetail] = {
    var keys = Map.empty[Class[_ <: PathDetail], PathDetail]
    for(key <- xs){
      if(!keys.contains(key.getClass)){
        keys = keys + (key.getClass -> key)
      }
    }

    keys.values.toList
  }

  protected def projectParser[_: P]: P[Path] = {
    import SingleLineWhitespace._

    def snippetPath: P[String] = P(CharsWhile(!_.isWhitespace).!)

    P(Start ~ keys ~ snippetPath ~ End)
      .map {
        case (keys, path) => Path(path, keys)
      }
  }

  def extractPathParts(path: String): Either[String, Path] = {
    parse(path, projectParser(_)) match {
      case Parsed.Success(value, _) => Right(value)
      case f@Parsed.Failure(_, index, _) => Left(s"could not parse path at col $index. $f")
    }
    //}
  }
}

object PathParser extends PathParser
