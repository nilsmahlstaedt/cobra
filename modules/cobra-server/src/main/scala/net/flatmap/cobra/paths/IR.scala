package net.flatmap.cobra.paths

import net.flatmap.cobra.Snippet

sealed trait SnippetPath{
  def name: String
}

sealed trait Branch {
  def children: List[SnippetPath]
}

abstract sealed class IR
