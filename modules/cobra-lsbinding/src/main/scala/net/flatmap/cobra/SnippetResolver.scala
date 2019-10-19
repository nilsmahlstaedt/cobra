package net.flatmap.cobra

import scala.io.{Codec, Source}

object SnippetResolver {

  /**
    * resolves the code references by snippets source, startLine and endLine attributes
    * @param s snippet to resolve
    * @return code represented by snippet
    */
  def getSourceLines(s: Snippet): List[String] ={
    val source = Source.fromFile(s.source.toFile.toString)//, Codec.UTF8.toString())
    val lines = source.getLines().slice(s.startLine, s.endLine+1).toList
    source.close()

    lines
  }


  /**
    * formats snippet into a concise one line representaiton
    * @param s snippet to transform
    * @return formatted string
    */
  def shortLine(s:Snippet): String = {
    val parent = s.parent.getOrElse("")

    s"${s.kind} - '$parent${s.name}' ${s.startLine}:${s.endLine} (${s.endLine-s.startLine+1})"
  }
}
