package net.flatmap.cobra

import java.nio.file.Path

import net.flatmap.cobra.languageserver.{LSInteraction, LSLauncher}
import net.flatmap.cobra.util.FileUtils
import org.eclipse.lsp4j.services.LanguageServer
import net.flatmap.cobra.util.WithResource._

import scala.util.Try

case class Project(projectRoot: Path, language: Language, srcRoots: List[Path])

object ProjectAnalyzer {

  implicit class ClosableLS(ls: LanguageServer) extends AutoCloseable {
    override def close(): Unit = ls.exit()
  }

  def analyze(project: Project): Try[List[Snippet]] = {

    for{
      ls <- LSLauncher.launch(project.language, project.projectRoot)
      snippets <- withResource(ClosableLS(ls))(_ => {
        FileUtils
          .findProjectFiles(project.language, project.projectRoot::project.srcRoots)
          .map(LSInteraction.analyzeProjectFiles(ls, _))
      })
    } yield {
      snippets
    }


    /*
    can't use this nicer style of for-comprehension, as the LS would not be closed properly if findProjectFiles() failed

    for{
      ls <- LSLauncher.launch(project.language, project.projectRoot)
      projectFiles <- withResource(ls)(_ => FileUtils.findProjectFiles(project.language, project.projectRoot::project.srcRoots))
      snippets = LSInteraction.analyzeProjectFiles(ls, projectFiles)
    } yield {
      ls.exit()
      snippets
    }
    */
  }

}
