package net.flatmap.cobra

import java.nio.file.attribute.PosixFilePermission
import java.nio.file._

import better.files.Dsl.SymbolicOperations
import better.files._
import com.typesafe.config.ConfigFactory

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}


/**
  * Created by martin on 03.02.16.
  */
object Cobra extends App {
  val version = "1.0.6"

  private def assume(cond: Boolean, msg: String) = if (!cond) {
    println("failed to initialize: " + msg)
    sys.exit()
  }

  private def printLogo() = {
    println( """            ____      _                     """)
    println( """           / ___|___ | |__  _ __ __ _ TM    """)
    println( """          | |   / _ \| '_ \| '__/ _` |      """)
    println( """          | |__| (_) | |_) | | | (_| |      """)
    println( """           \____\___/|_.__/|_|  \__,_|      """)
    println( """______________________________________________""")
    println(s"""| version $version - (c) 2016-2019 Martin Ring |""")
    println()
  }

  if ((args.length == 1 || args.length == 2) && args.head == "new") {
    printLogo()
    val name = if (args.length > 1) args(1) else {
      scala.io.StdIn.readLine("please enter a name for the new presentation: ")
    }
    println(s"creating new cobra presentation '$name'...")
    val dir = File(name)
    assume(!dir.exists() || !dir.isDirectory, s"directory '$name' exists already")
    assume(dir.createDirectories().exists, "could not create directory")

    val conf = scala.io.Source.fromURL(getClass.getResource("/template-cobra.conf")).mkString
      .replaceAll("\\{\\s*title\\s*\\}", s""""$name"""")
      .replaceAll("\\{\\s*lang\\s*\\}", '"' + System.getProperty("user.language") + '"')
    val slides = scala.io.Source.fromURL(getClass.getResource("/template-slides.html")).mkString
      .replaceAll("\\{\\s*title\\s*\\}", name)

    import better.files.InputStreamExtensions
    val pandocFilterRes = getClass.getResourceAsStream("/projectDefFilter").byteArray

    (dir / "cobra.conf").createIfNotExists() < (conf)
    (dir / "slides.html").createIfNotExists() < (slides)
    (dir / ".projectDefFilter")
        .createFileIfNotExists()
        .addPermission(PosixFilePermission.OTHERS_EXECUTE)
        .writeByteArray(pandocFilterRes)

    println("the presentation has been successfully initialized.")
    println(s"you may start presentation with 'cobra $name'")

    sys.exit()
  }

  if (args.length > 1) {
    println(s"invalid arguments: '${args.mkString(" ")}'")
    sys.exit(-1)
  }

  val directory = if (args.isEmpty) File(".") else File(args.head)

  // taken from CobraServer
  // TODO refactor copied code from CobraServer
  def readConfig() = ConfigFactory.parseFile(
    (directory / "cobra.conf").toJava
  ).withFallback(ConfigFactory.load().getConfig("cobra"))

  def generateSlides(): Unit = {
    val conf = readConfig()
    val pandocPath = conf.getString("markdown.pandoc.path")

    val filterPath = {
      val p = Paths.get(conf.getString("markdown.filter.path"))

      if(p.isAbsolute){
        p.toString
      }else{
        (directory / p.toString).path.toAbsolutePath.toString
      }
    }
    assert(File(filterPath).exists, s"could not find pandoc filter in path '$filterPath'")


    val pandocArgs = List(
      "--mathjax",
      //"--standalone",
      //"--slide-level=2",
      "--from markdown+fenced_code_attributes",
      "--to revealjs",
      s"--filter ${filterPath}",
      s"--output ${(directory/"slides.html").createFileIfNotExists().path.toString}",
      (directory / "slides.md").path.toString
    )

    import sys.process._
    val command = s"${pandocPath} ${pandocArgs.mkString(" ")}"
    println(s"pandoc command: '$command'")
    val result = command.!

    assert(result == 0, s"could not generate slides from markdown. pandoc exited with $result")
  }

  def initSlides(): Unit = {
    assert((directory / "slides.html").exists || (directory / "slides.md").exists, "no slides.html nor lsides.md found")

    (
      Try{(directory / "slides.html").lastModifiedTime},
      Try{(directory / "slides.md").lastModifiedTime}
    ) match {
    //case (Failure, Failure) is guarded by assert in first line of function
      case (Failure(_), Success(_)) => generateSlides()
    //case (Success(_), Failure) => do nothing
      case (Success(htime), Success(mtime)) if mtime.isAfter(htime) => generateSlides()
    //case (Success(htime), Success(mtime)) if htime.isAfter(mtime) => // do nothing
      case _ => ()

    }
    /*
    Authors Note:
    echo "foobar" >> slides.md -> does trigger file change event
    open in geddit + change + save -> no trigger, but change in .goutputstream-D5JL8Z

    i have no idea why this happens
     */
    val slidesMd = (directory / "slides.md")
    val watchService = FileSystems.getDefault.newWatchService()
    directory.path.register(watchService, StandardWatchEventKinds.ENTRY_MODIFY)
    directory.path.register(watchService, StandardWatchEventKinds.ENTRY_CREATE)

    Future{
      println(s"start watching ${directory.path} for changes to ${slidesMd.name}${slidesMd.extension(includeDot = true)}")

      while(true){
        val key = watchService.take()

        for(event <- key.pollEvents().asScala.toList){
          event match {
            case x:WatchEvent[Path] if x.context().getFileName.equals(slidesMd.path.getFileName) => {
              println("update of slides.md detected!")
              println("regenerating slides!")
              generateSlides()
            }
            case _ => ()
          }
        }

        key.reset()
      }
    }
  }

  { // initialize
    printLogo()
    assume(directory.exists, "could not find " + directory.toString())
    assume(directory.isDirectory, directory.toString() + " is not a directory")
    assume(directory.isReadable, "can not read " + directory.toString())
    assume((directory / "cobra.conf").exists(), "no cobra.conf found")

    initSlides()
    assume((directory / "slides.html").exists(), "no slides.html found")

    val server = new CobraServer(directory)
    server.start()
    while (scala.io.StdIn.readLine != "exit") ()
    server.stop()
  }
}
