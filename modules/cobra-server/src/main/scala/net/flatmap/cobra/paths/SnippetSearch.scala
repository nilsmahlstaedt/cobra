package net.flatmap.cobra.paths

import net.flatmap.cobra.Snippet

/**
 * makes list of snippets searchable
 */
trait SnippetSearch{

  private def splitPath(s: String): List[String]  = {
    s.split('/').toList.map(_.trim.toLowerCase).filter(_.nonEmpty)
  }

  private def joinPath(s: List[String]): String = {
    s.mkString("/")
  }

  implicit class SnippetDictionary(snippets: List[Snippet]) {
    def findSnippets(p:Path): List[Snippet] = {
      val typed: List[Snippet] = snippets
        .filter(s => p.typeBound().forall(bound => s.kind.equals(bound.typ)))


      val splitted = typed.map(Path.buildPathString).map(splitPath)
      val pSplit = splitPath(p.path)

      if(p.isAbsolute){
        // path is absolute match front to end
        typed.filter(s => {
          val sSplit: List[String] = splitPath(Path.buildPathString(s))
          sSplit.startsWith(pSplit)
        })
      }else{
        // path is just a partial path, match from end
        typed.filter(s => {
          val sSplit: List[String] = splitPath(Path.buildPathString(s))
          sSplit.containsSlice(pSplit)
        })
      }
    }
  }
}
