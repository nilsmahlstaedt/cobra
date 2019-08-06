package net.flatmap.cobra.util

import java.io.File
import java.nio.file.{Files, Path}
import java.util.stream.Collectors

import net.flatmap.cobra.Language
import net.flatmap.cobra.util.WithResource._

import scala.jdk.CollectionConverters._
import scala.io.Source
import scala.util.Try

object FileUtils {

  /**
    * get file contents
    * @param f file to
    * @return
    */
  def getContent(f: File): String = {
    withResource(Source.fromFile(f)){s =>
      s.getLines().mkString("\n")
    }
  }

  /**
    * recursively searches through baseDir and subdirs for project files based n file extensions
    * returning all files matching the specified extension (case insensitive)
    *
    * Does not return links, dirs and non readable files
    * Does not follow links
    * May return duplicates
    *
    * @param srcRoots base dirs of source files
    * @param language language of source files. file extension of language will be suffix matched (case-insensitive) on filename
    * @return list of all files with matching file extensions in and below base dir
    */
  def findProjectFiles(language: Language, srcRoots: List[Path] ): Try[List[Path]] = Try {

    srcRoots.flatMap(root => {
      //TODO when upgrading to scala 2.13 rewrite this block with "Using" https://scala-lang.org/files/archive/api/2.13.0/scala/util/Using$.html
      withResource(Files.walk(root)){walk =>
        walk
          .filter(p => {
            val f = p.toFile

            f != null &&
              f.exists() &&
              f.isFile &&
              f.canRead &&
              f.getName.toLowerCase.endsWith(language.fileExtension.toLowerCase)
          }).collect(Collectors.toList()).asScala
      }

    })
  }

}
