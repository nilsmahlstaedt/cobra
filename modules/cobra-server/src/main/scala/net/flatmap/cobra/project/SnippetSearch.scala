package net.flatmap.cobra.project

import net.flatmap.cobra.Snippet
import net.flatmap.cobra.paths.{Path, ProjectAssociation, TypeBound}
import org.eclipse.lsp4j.SymbolKind

/**
 * makes list of snippets searchable
 */
trait SnippetSearch{
  implicit class SnippetDictionary(snippets: List[Snippet]) {
    def findSnippets(p:Path): List[Snippet] = {
      val typed: List[Snippet] = snippets
        .filter(s => p.typeBound().forall(bound => s.kind.equals(bound.typ)))


      if(p.isAbsolute){
        // path is absolute match front to end
        typed.filter(s => {
          val snippetPath = Path.buildPathString(s)
          snippetPath.trim.toLowerCase.startsWith(p.path.trim.toLowerCase)
        })
      }else{
        // path is just a partial path, match from end
        typed.filter(s => {
          val snippetPath = Path.buildPathString(s).toLowerCase.trim
          snippetPath.toLowerCase.contains(p.path.trim)
        })
      }
    }
  }
}