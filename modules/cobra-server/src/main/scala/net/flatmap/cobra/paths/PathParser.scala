package net.flatmap.cobra.paths

import fastparse._
import net.flatmap.cobra.project.TypeNames
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
  protected def typeBound[_:P]: P[TypeBound] = {
    //import NoWhitespace._
    //def nameCS = StringIn(TypeNames.names:_*).!

    //def fixableNames = StringInIgnoreCase(TypeNames.singleWords:_*).!.map(_.toLowerCase.capitalize)

    def name: P[String] =
      TypeNames.names
        .map(s =>
          P(IgnoreCase(s)).map(_ => s)
        ) match {
        case Nil => Fail //can parse if no words are allowed
        case x::Nil => x
        case x::xs => xs.foldLeft(x)((acc, p) => P(acc | p))
      }

    //def name: P[String] = nameCS | fixableNames

    P(name)
      .map(n => Try(SymbolKind.valueOf(n)))
      .filter(_.isSuccess)
      .map(_.get)
      .map(TypeBound.apply)
  }

  protected def projectAssociation[_:P]: P[ProjectAssociation] = {
    P(CharsWhile(_.isLetterOrDigit).!)
      .map(ProjectAssociation.apply)
  }

  protected def projectParser[_: P]: P[Path] = {
    import SingleLineWhitespace._

    def key[V](tagChar: String, content: P[V]): P[Option[V]] = P(("[" ~ tagChar ~ ":" ~ content  ~ "]").?)

    def projectKey: P[Option[PathDetail]] = key("p", projectAssociation)
    def typeKey: P[Option[PathDetail]] = key("t", typeBound)

    def gobbleBrokenKeys = P("[" ~ CharsWhile(_ != ']') ~ "]").rep(0)

    def snippetPath: P[String] = P(CharsWhile(!_.isWhitespace).!)

    P(Start ~ projectKey ~ typeKey ~ gobbleBrokenKeys ~ snippetPath ~ End)
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

object PathParser extends PathParser
