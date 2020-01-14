package net.flatmap.cobra

import better.files.File
import com.typesafe.config.{Config, ConfigFactory}

object Configuration {
  def read(fromDir: File): Config = ConfigFactory.parseFile(
    (fromDir / "cobra.conf").toJava
  ).withFallback(ConfigFactory.load().getConfig("cobra"))
}
