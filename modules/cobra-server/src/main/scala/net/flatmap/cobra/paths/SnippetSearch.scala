package net.flatmap.cobra.paths

import net.flatmap.cobra.Snippet

import scala.collection.SeqOps

/**
 * makes list of snippets searchable
 */
trait SnippetSearch{

  private def splitPath(s: String): List[String]  = {
    s.split('.').toList.map(_.trim.toLowerCase).filter(_.nonEmpty)
  }

  private def joinPath(s: List[String]): String = {
    s.mkString("/")
  }

  implicit class SnippetDictionary(snippets: List[Snippet]) {
    def findSnippets(p:Path): List[Snippet] = {
      val typed: List[Snippet] = snippets
        .filter(s => p.typeBound().forall(bound => s.kind.equals(bound.typ)))

      val pSplit = splitPath(p.path)

        typed.filter(s => {
          val sSplit: List[String] = splitPath(Path.buildPathString(s))
          sSplit.containsSlice(pSplit)
        })
    }
  }
}
