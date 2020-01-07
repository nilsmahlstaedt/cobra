package net.flatmap.cobra.languageserver

import java.nio.file.Path

import net.flatmap.cobra.Snippet
import net.flatmap.cobra.util.{FileUtils, LSConverters}
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.lsp4j._

import scala.util.{Failure, Success, Try}

object LSInteraction extends LSConverters {

  def analyzeProjectFiles(ls: LanguageServer, files: List[Path]): List[Snippet] = {
    files
      .map(analyzeFile(ls))
      .flatMap {
        case Success(snippets) => snippets
        case Failure(ex) =>
          println(ex.getLocalizedMessage)
          Nil
      }
  }

  def analyzeFile(ls: LanguageServer)(f: Path): Try[List[Snippet]] = Try {
    val docURI = f.toUri.toString

    ls.getTextDocumentService.didOpen(
      new DidOpenTextDocumentParams(
        new TextDocumentItem(
          docURI,
          "scala",
          0,
          FileUtils.getContent(f.toFile)
        )))

    //get symbols
    val symbolResp = ls.getTextDocumentService.documentSymbol(
      new DocumentSymbolParams(
        new TextDocumentIdentifier(docURI)
      )
    ).convertToScala

    //close document
    ls.getTextDocumentService.didClose(new DidCloseTextDocumentParams(new TextDocumentIdentifier(docURI)))
    //println(s"analysed: ${f.toAbsolutePath}")

    symbolResp.flatMap(Snippet.apply(f, _))
  }
}
