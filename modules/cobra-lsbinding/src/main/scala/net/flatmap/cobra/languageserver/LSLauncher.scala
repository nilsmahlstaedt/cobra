package net.flatmap.cobra.languageserver
import java.io._
import java.nio.file.Path

import net.flatmap.cobra.util.PID
import net.flatmap.cobra.{Language, NoOpClient}
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.LanguageServer

import scala.jdk.CollectionConverters._
import scala.concurrent.Future
import scala.util.Try

object LSLauncher {

  import scala.concurrent.ExecutionContext.Implicits.global

  case class LSPProcess(process: Process, stdIn: OutputStream,stdOut: InputStream, stdErr: InputStream)

  def launch(language: Language,
              projectRoot: Path,
              pid: Option[Long] = None,
              logErrorOut: Boolean = false,
              printDebugOutput: Boolean = false ): Try[LanguageServer] = Try{

    val conf = language.lsConfig
    println(conf)
    val proc = startProcess(conf.binary, conf.args)

    if(logErrorOut){
      forwardToStdout(proc.stdErr)
    }

    //launch server
    val launcher: Launcher[LanguageServer] = LSPLauncher.createClientLauncher(new NoOpClient(printDebugOutput), proc.stdOut, proc.stdIn)
    launcher.startListening()


    val server: LanguageServer = launcher.getRemoteProxy

    println("initialize!")
    val initializationResult = server.initialize(
      generateInitParams(pid.getOrElse(PID.get()).toInt, projectRoot, printDebugOutput)
    ).get
    println(s"initialize result: $initializationResult")

    println("initialized!")
    server.initialized(new InitializedParams())

    server
  }

  private def forwardToStdout(in: InputStream): Future[Unit] = {
    Future{
      new BufferedReader(new InputStreamReader(in)).lines().forEach(println)
    }
  }

  private def startProcess(cmd: String, args: List[String]) : LSPProcess = {

    println(s"""starting process with cmd="$cmd"""")

    // TODO synchronize JPB Access globally!
    // TODO suppress IO for java ProcessBuilder
    val process: Process = new ProcessBuilder((cmd :: args).asJava).start()

    LSPProcess(process, process.getOutputStream, process.getInputStream, process.getErrorStream)
  }


  /**
    * generates a fresh initialization parameter
    * @param pid process id of parent process (monitored for death by LS)
    * @param projectRoot root dir URI of project
    * @param verboseLogging use verbose logging?
    * @param customOptions LS-Specific custom options
    */
  private def generateInitParams(pid: Int, projectRoot: Path, verboseLogging: Boolean = false, customOptions: Object = new Object()): InitializeParams = {
    val wsc = new WorkspaceClientCapabilities()
    val tdc = new TextDocumentClientCapabilities()

    tdc.setDocumentSymbol(new DocumentSymbolCapabilities(new SymbolKindCapabilities(SymbolKind.values().toList.asJava)))

    val initP = new InitializeParams()
    initP.setProcessId(pid)
    initP.setRootUri(projectRoot.toAbsolutePath.toUri.toString)

    if(verboseLogging){
      initP.setTrace("verbose")
    }

    initP.setCapabilities(new ClientCapabilities(wsc, tdc, customOptions))

    initP
  }
}
