package net.flatmap.cobra

import net.flatmap.cobra.languageserver.StartupConf

abstract class Language(val id: String, val fileExtension: String, val lsConfig: StartupConf)


object Languages {

  //supported languages
  case class Haskell() extends Language("haskell", ".hs", StartupConf.Haskell)
  case class Scala() extends Language("scala", ".scala", StartupConf.Scala)



  private val mapping: PartialFunction[String, Language] = {
    case "haskell" => Haskell()
    case "scala" => Scala()
  }

  def fromString(languageId: String): Option[Language] = {
    mapping.lift.apply(languageId)
  }
}
