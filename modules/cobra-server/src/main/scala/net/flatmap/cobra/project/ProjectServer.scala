package net.flatmap.cobra.project

import java.nio.file.{Path, Paths, StandardWatchEventKinds}

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import better.files.FileWatcher._
import better.files._
import net.flatmap.cobra._
import net.flatmap.cobra.languageserver.{LSInteraction, LSLauncher}
import net.flatmap.cobra.paths.SnippetSearch
import net.flatmap.cobra.project.ProjectServer.ProjectFileUpdate
import net.flatmap.cobra.project.SearchActor.{RequestSnippets, SnippetResponse}
import net.flatmap.cobra.util.FileUtils
import org.eclipse.lsp4j.services.LanguageServer

import scala.util.{Success, Try}

class ProjectServer(projectId: String, pid: Long, language: Language, mode: Mode, rootPath: Path, srcRoots: List[Path]) extends Actor with ActorLogging with SnippetSearch {

  implicit class IfEmptyList[A](l: List[A]) {
    def orIfEmpty(other: A): List[A] = orIfEmpty(List(other))

    def orIfEmpty(other: List[A]): List[A] = if (l.isEmpty) {
      other
    } else {
      l
    }
  }

  /**
   * holds language server to be exited when actor dies
   */
  private var languageServerPlaceholder: Option[LanguageServer] = None
  private var filewatches: List[ActorRef] = Nil

  override def preStart(): Unit = {
    super.preStart()

    implicit val system: ActorSystem = context.system
    log.info(s"Project Server $projectId initializing!")

    log.debug("start language server")
    (for {
      ls: LanguageServer <- LSLauncher.launch(language, rootPath, Some(pid))
    } yield {
      languageServerPlaceholder = Some(ls)

      log.debug("finding project files")
      val projectFiles = FileUtils.findProjectFiles(language, srcRoots.orIfEmpty(rootPath)).getOrElse(Nil).distinct

      if(log.isDebugEnabled){
        log.debug(s"analyzing ${projectFiles.size} project files:")
        projectFiles.foreach(f => println(s"$projectId:${f.toAbsolutePath}"))
      }
      val snippets: List[Snippet] = LSInteraction.analyzeProjectFiles(ls, projectFiles)

      log.debug("creating file watches")
      snippets
        .flatMap(s => Option(s.source.normalize().getParent).map((_, s.source)))
        .groupMap(_._1)(_._2)
        .foreach {
          case (k, v) => watchFiles(k, v)
        }

      log.info(s"ProjectServer for project $projectId is initialized with ${snippets.length} snippets")

      if(log.isDebugEnabled){
        snippets.foreach(s => log.debug(s"[$projectId][t:${s.kind}] ${net.flatmap.cobra.paths.Path.buildPathString(s)}"))
      }

      context.become(running(ls, snippets))
    }) recover {
      case e: Throwable =>
        // ensure a single mis-defined LS does not bring down queries
        log.error(e, s"could not start project server for project '$projectId'")
        context.become(faulty())
    }

    log.debug(s"Project Server $projectId initialized")
  }

  override def postStop(): Unit = {
    super.postStop()

    //stop filewatches
    filewatches.foreach(watcher => {
      context.stop(watcher)
    })

    // stop ls
    languageServerPlaceholder.foreach(server =>{
      server.shutdown().get()
      server.exit()
    })
  }

  /**
   * creates filewatch for snippetPaths in directory dir
   *
   * sends watch events to self
   * @note do not create one watch for every file in a dir as you will run
   *       into the systems ionotify limit
   *
   * @param dir          directory path to watch for changes
   * @param snippetPaths path to watch out for in dir
   */
  private def watchFiles(dir: Path, snippetPaths: List[Path])(implicit sys: ActorSystem): Unit = {
    val ref = context.actorOf(File(dir).watcherProps(recursive = false))
    filewatches = ref :: filewatches

    ref ! when(
      StandardWatchEventKinds.ENTRY_CREATE,
      StandardWatchEventKinds.ENTRY_DELETE,
      StandardWatchEventKinds.ENTRY_MODIFY) {
      case (event, file) if snippetPaths.contains(file.path) =>
        log.debug(s"${context.self.path.name} got file update for existing file $file")
        self ! ProjectFileUpdate(event, file)
      case (event, file) if mode.fileendings.contains(file.extension.getOrElse("")) =>
        log.debug(s"${context.self.path.name} got file update for new file $file")
        self ! ProjectFileUpdate(event, file)
      case (event, file) =>
        log.debug(s"IGNORED - file update $event for $file")
    }
  }

  override def receive: Receive = {
    case e => log.warning(s"received message before initializing! msg:$e")
  }

  def faulty(): Receive = {
    case InitProject(`projectId`, _, _, _) =>
      log.warning(s"could not start project server '$projectId'")
      log.debug("sending Default Project Initialized Value")
      sender() ! ProjectInitialized(projectId)
    case _:RequestSnippets =>
      log.warning(s"could not start language server for project server '$projectId', answering to query with 0 results")
      sender() ! SnippetResponse(projectId, mode, Nil)
  }

  def running(ls: LanguageServer, dict: List[Snippet]): Receive = {
    case InitProject(`projectId`, _, _, _) => sender() ! ProjectInitialized(projectId)
    case RequestSnippets(path) => sender() ! SnippetResponse(projectId, mode, dict.findSnippets(path))

    // update dict when project files change
    case ProjectFileUpdate(StandardWatchEventKinds.ENTRY_CREATE, file) =>
      LSInteraction.analyzeFile(ls)(file.path).foreach(newSnippets => context.become(running(ls, dict ++ newSnippets)))
    case ProjectFileUpdate(StandardWatchEventKinds.ENTRY_DELETE, file) =>
      val pathToRemove = file.path
      context.become(running(ls, dict.filterNot(_.source.equals(pathToRemove))))
    case ProjectFileUpdate(StandardWatchEventKinds.ENTRY_MODIFY, file) =>
      val pathOfUpdate = file.path
      val oldRemoved = dict.filterNot(_.source.equals(pathOfUpdate))
      val newSnippets = LSInteraction.analyzeFile(ls)(pathOfUpdate).toOption.toList.flatten
      context.become(running(ls, oldRemoved ++ newSnippets))

    case e => log.error(s"received unknown message: $e")
  }
}

object ProjectServer {

  case class ProjectFileUpdate(event: Event, file: File)

  /**
   * generate props for a new project server
   *
   * @param pid      cobra main thread pid (watched by LS for auto exit if dead)
   * @param id       project id
   * @param mode     language
   * @param root     project root dir
   * @param srcRoots src dir roots
   *                 (will be resolved relative to project root if given in relative form)
   * @return
   */
  def props(pid: Long, id: String, mode: Mode, root: String, srcRoots: List[String] = Nil): Try[Props] = {
    for {
      language <- Try {
        Languages.fromString(mode.name).get
      }
      rootPath <- Try {
        Paths.get(root).toAbsolutePath
      }
      srcRootPaths = srcRoots.map(sr => Try {
        Paths.get(sr) match {
          case p if p.isAbsolute => p
          case p => rootPath.resolve(p)
        }
      }).collect {
        case Success(p) => p
      }
    } yield {
      Props(new ProjectServer(id, pid, language, mode, rootPath, srcRootPaths))
    }
  }
}