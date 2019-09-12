package net.flatmap.cobra.project

import net.flatmap.cobra.Snippet
import net.flatmap.cobra.paths.{Path, ProjectAssociation, TypeBound}
import org.eclipse.lsp4j.SymbolKind

/**
 * contains all snippets for one project
 * @param snippets snippets to include in the dictionary
 */
class SnippetDictionary(snippets: List[Snippet]) {

  private type TypeDict = Map[String, List[Snippet]]
  private type SnippetDict = Map[SymbolKind, TypeDict]

  val maps: SnippetDict  = snippets.groupBy(_.kind).toList.map{
    case (kind, snpts) => (kind, snpts.groupBy(Path.buildPathString))
  }.toMap

  def find(path:Path): List[Snippet] = {
    // add your details that are sub project level to this collect statement
    val typed: Iterable[TypeDict] = path.typeBound() match {
      case Some(TypeBound(typ)) => maps.get(typ)
      case None => maps.values
    }

    typed.flatMap(dict => dict.get(path.path)).flatten.toList
  }
}