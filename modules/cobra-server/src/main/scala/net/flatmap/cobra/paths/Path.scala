package net.flatmap.cobra.paths

import org.eclipse.lsp4j.SymbolKind

sealed abstract class PathDetail
case class ProjectAssociation(project: String) extends PathDetail
case class TypeBound(typ: SymbolKind) extends PathDetail

case class Path(path: String, details: List[PathDetail]){
  def isAbsolute: Boolean = path.startsWith("..")

  def projectAssociation(): Option[ProjectAssociation] = details.collectFirst {
    case x: ProjectAssociation => x
  }

  def typeBound(): Option[TypeBound] = details.collectFirst{
    case x: TypeBound => x
  }
}

object Path {
  def apply(path: String, details: PathDetail*) = new Path(path, details.toList)
}
