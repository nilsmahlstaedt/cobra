package net.flatmap.cobra

import java.net.URI
import java.nio.file.{Path, Paths}

import org.eclipse.lsp4j.{DocumentSymbol, SymbolInformation, SymbolKind}

case class Snippet(name: String, parent: Option[String], source: Path, startLine: Int, endLine: Int, kind: SymbolKind)

object Snippet {
  def apply(e: Either[SymbolInformation, DocumentSymbol]): Snippet  = e match {
  case Left(s) => Snippet(
    name = s.getName,
    parent = Option(s.getContainerName),
    source = Paths.get(URI.create(s.getLocation.getUri).getPath),
    startLine = s.getLocation.getRange.getStart.getLine,
    endLine = s.getLocation.getRange.getEnd.getLine,
    kind = s.getKind
  )
  case Right(d) => ??? //so far i haven't encountered a Document Symbol
  }
}