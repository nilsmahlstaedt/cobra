package net.flatmap.cobra

import java.io.File
import java.net.URI
import java.nio.file.{Path, Paths}

import scala.jdk.CollectionConverters._
import org.eclipse.lsp4j.{DocumentSymbol, SymbolInformation, SymbolKind}

import scala.annotation.tailrec

case class Snippet(name: String, parent: Option[String], source: Path, startLine: Int, endLine: Int, kind: SymbolKind)

object Snippet {
  def apply(source: Path, e: Either[SymbolInformation, DocumentSymbol]): List[Snippet]  = e match {
  case Left(s) => Snippet(
    name = s.getName,
    parent = Option(s.getContainerName),
    source = Paths.get(URI.create(s.getLocation.getUri).getPath),
    startLine = s.getLocation.getRange.getStart.getLine,
    endLine = s.getLocation.getRange.getEnd.getLine,
    kind = s.getKind
  ) :: Nil
  case Right(d) => buildSnippets(d, "", source)//so far i haven't encountered a Document Symbol
      //TODO implement behaviour
  }

  private def dsToS(ds: DocumentSymbol, path: String, source: Path): Snippet = {
    Snippet(
      ds.getName,
      Some(path),
      source,
      ds.getRange.getStart.getLine,
      ds.getRange.getEnd.getLine,
      ds.getKind
    )
  }

  private def buildSnippets(symbol: DocumentSymbol, path: String = "", source: Path): List[Snippet] = {
    val snippet = dsToS(symbol, path, source)
    val children = symbol.getChildren.asScala.toList

    snippet :: children.flatMap(buildSnippets(_, path+"/"+snippet.name, source))
  }
}