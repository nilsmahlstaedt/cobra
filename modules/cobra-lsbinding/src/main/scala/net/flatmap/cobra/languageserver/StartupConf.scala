package net.flatmap.cobra.languageserver

import net.flatmap.cobra.BindingConfig

case class StartupConf(language: String, binary: String, args: List[String])


object StartupConf {
  // default lang server for testing
  val Haskell: StartupConf = BindingConfig.languageServers.getOrElse("haskell", StartupConf("haskell", "hie-wrapper", List("--lsp")))
  val Scala: StartupConf = BindingConfig.languageServers.getOrElse("scala", StartupConf("scala", "metals", Nil))

  def forLanguage(lang: String): Option[StartupConf] = BindingConfig.languageServers.get(lang.toLowerCase())
}
