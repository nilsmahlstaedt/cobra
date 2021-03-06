package net.flatmap.cobra
import scala.concurrent.ExecutionContext.Implicits.global
import net.flatmap.js.util.{NodeSeqQuery, RVar}
import org.scalajs.dom.{Element, console, raw}

import scala.collection.mutable
import scala.concurrent.{Future, Promise}

object Projects {

  var pendingInits: RVar[Set[String]] = RVar(Set.empty)

  /**
   * clear id's or pending project initialisations
   */
  def resetPendingInits(): Unit = {
    pendingInits = RVar(Set.empty)
  }

  def initProjects(root: NodeSeqQuery): Future[Unit] = {
    console.info("initializing projects")

    root.query("div[data-key][data-language][data-root][data-srcRoots]").elements.foreach { project =>
      //select all nodes with 'project-definition' class
      val key = project.getAttribute("data-key")
      val mode = Mode.modes.find(_.name.equals(project.getAttribute("data-language").toLowerCase))
      val root = project.getAttribute("data-root")
      val srcRoots = project.getAttribute("data-srcRoots").split(',').toList

      mode match {
        case Some(m) =>
          CobraJS.send(InitProject(key, m, root, srcRoots))
          pendingInits.modify(_ + key)
        case None => console.error(s"could not initialize project for language ${project.getAttribute("data-language")}")
      }
    }

    val p = Promise.apply[Unit]()

    pendingInits.react(remaining => {
      console.info(s"waiting for ${remaining.size} projects to initialize")
      if(remaining.isEmpty) p.success(())
    })

    val fut = p.future

    fut.onComplete(_ =>
      console.info("project initialization was completed!")
    )

    // should there be no projects to init complete the promise right now!
    if(pendingInits().isEmpty) p.success(())

    fut
  }
}
