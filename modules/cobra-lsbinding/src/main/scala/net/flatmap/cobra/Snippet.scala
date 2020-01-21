package net.flatmap.cobra

import java.io.File
import java.net.URI
import java.nio.file.{Path, Paths}
import scala.jdk.CollectionConverters._
import org.eclipse.lsp4j.{DocumentSymbol, SymbolInformation, SymbolKind}

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
    case Right(d) => d.toSnippets("", source)
  }

  /**
   * Visitor for DocumentSymbols
   * offers visitor pattern based conversion to a flattened list of snippets
   * @param ds Document Symbold
   */
  private implicit class DocumentSymbolVisitor(ds: DocumentSymbol) {
    def toSnippets(parentPath: String, source: Path): List[Snippet] = {
      val snippet: Snippet = Snippet(
        ds.getName,
        Some(parentPath),
        source,
        ds.getRange.getStart.getLine,
        ds.getRange.getEnd.getLine,
        ds.getKind
      )

      val children: List[DocumentSymbol] = ds.getChildren.asScala.toList

      snippet :: children.flatMap(_.toSnippets(parentPath+"/"+snippet.name, source))
    }
  }
}