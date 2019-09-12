package net.flatmap.cobra.project

import net.flatmap.cobra.Snippet
import net.flatmap.cobra.paths.{Path, ProjectAssociation, TypeBound}
import org.eclipse.lsp4j.SymbolKind

/**
 * contains all snippets for one project
 * @param snippets snippets to include in the dictionary
 */
class SnippetDictionary(snippets: List[Snippet]) {

  var maps: Map[SymbolKind, Map[String, Snippet]] = Map.empty

  

  def find(path:Path): List[Snippet] = {
    // add your details that are sub project level to this collect statement
    val predicates = path.details.collect{
      case TypeBound(typ) => (s:Snippet) => s.kind.equals(typ)
    }

    //combine filter to single test
    val filterFunc: Snippet => Boolean = (s:Snippet) =>
      predicates.forall(test => test(s))
  }

  snippets.filter(filterFunc)
}

trait PredicateListComposition {
  implicit class PredicateList[T](l: List[T => Boolean]) {
    def composeWithAnd: T => Boolean = (x: T) => l.forall(pred => pred(x))
  }
}