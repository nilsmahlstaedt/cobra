package net.flatmap.cobra

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import net.flatmap.cobra.languageserver.StartupConf

import scala.jdk.CollectionConverters._

object BindingConfig {
  private val root: Config = ConfigFactory.load().getConfig("lsp-binding")

  val languageServers: Map[String, StartupConf] = root.getObject("language-server")
    .asScala
    .keys
    .map(key => {
      val obj = root.getConfig(s"language-server.$key")
      val binary = obj.getString("binary")
      val args = obj.getStringList("args").asScala.toList

      (key, StartupConf(key, binary, args))
    }).toMap


  val outputDir = Paths.get(root.getString("outputDir"))

}
