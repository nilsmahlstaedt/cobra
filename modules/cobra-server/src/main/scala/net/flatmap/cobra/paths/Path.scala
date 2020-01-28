package net.flatmap.cobra.paths

import net.flatmap.cobra.Snippet
import org.eclipse.lsp4j.SymbolKind

sealed abstract class PathDetail
case class ProjectAssociation(project: String) extends PathDetail
case class TypeBound(typ: SymbolKind) extends PathDetail

case class Path(path: String, details: List[PathDetail]){
  //def isAbsolute: Boolean = path.startsWith("/")

  def projectAssociation(): Option[ProjectAssociation] = details.collectFirst {
    case x: ProjectAssociation => x
  }

  def typeBound(): Option[TypeBound] = details.collectFirst{
    case x: TypeBound => x
  }

  override def toString: String = {
    val proj = projectAssociation().map(p => s"[${p.project}] ").getOrElse("")
    val typ = typeBound().map(t => s"[${t.typ}] ").getOrElse("")

    proj+typ+path
  }
}

object Path {
  def apply(path: String, details: PathDetail*) = new Path(path, details.toList)

  def normalize(path: String): String = {
    PathParser.producePathElems(path).mkString(".")
  }

  def getPathElements(s: Snippet): List[String] = producePathElements(s.parent, s.name)

  def buildPathString(s: Snippet): String = getPathElements(s).mkString(".")

  private def producePathElements(parent: Option[String], name: String): List[String] = {
    val parentElems = parent.map(PathParser.producePathElems).getOrElse(Nil)
    val nameElems = PathParser.producePathElems(name)

    parentElems ::: nameElems
  }
}
