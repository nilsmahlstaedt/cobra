package net.flatmap.cobra.project

import net.flatmap.cobra.Snippet
import net.flatmap.cobra.paths.{Path, ProjectAssociation, TypeBound}
import org.eclipse.lsp4j.SymbolKind

/**
 * contains all snippets for one project
 * @param snippets snippets to include in the dictionary
 */
class SnippetDictionary(snippets: List[Snippet]) {

  def find(path:Path): List[Snippet] = {
    val typed: List[Snippet] = snippets.filter(s => path.typeBound().forall(bound => s.kind == bound.typ))
    if(path.isAbsolute){
      // path is absolute match front to end
      typed.filter(s => {
        val snippetPath = Path.buildPathString(s)
        path.path.equalsIgnoreCase(snippetPath)
      })
    }else{
      // path is just a partial path, match from end
      typed.filter(s => {
        val snippetPath = Path.buildPathString(s).toLowerCase
        snippetPath.toLowerCase.endsWith(path.path)
      })
    }
  }
}