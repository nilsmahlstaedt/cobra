package net.flatmap.cobra
import scala.concurrent.ExecutionContext.Implicits.global
import net.flatmap.js.util.{NodeSeqQuery, RVar}
import org.scalajs.dom.{Element, console, raw}

import scala.collection.mutable
import scala.concurrent.{Future, Promise}

object Projects {

  //TODO replace with a set of project id's?
  val initsRemainging: RVar[Int] = RVar.apply(0)

  def initProjects(root: NodeSeqQuery): Future[Unit] = {
    console.info("initializing projects")

    //reset inits
    initsRemainging := 0

    root.query("div[data-key][data-language][data-root][data-srcRoots]").elements.foreach { project =>
      //select all nodes with 'project-definition' class
      val key = project.getAttribute("data-key")
      val mode = Mode.modes.find(_.name.equals(project.getAttribute("data-language").toLowerCase))
      val root = project.getAttribute("data-root")
      val srcRoots = project.getAttribute("data-srcRoots").split(',').toList

      mode match {
        case Some(m) =>
          CobraJS.send(InitProject(key, m, root, srcRoots))
          initsRemainging := initsRemainging() + 1
        case None => console.error(s"could not initialize project for language ${project.getAttribute("data-language")}")
      }
    }

    console.info(s"waiting for initialization of ${initsRemainging()} projects")


    val p = Promise.apply[Unit]()
    initsRemainging.react(remaining => {
      console.info(s"reacting to projects RVar change to: $remaining")
      if(remaining == 0) p.success(())
    })

    p.future.onComplete(_ =>
      console.info("promise of project initialization was completed!")
    )

    // should there be no projects to init complete the promise right now!
    if(initsRemainging() == 0) p.success(())

    p.future

  }
}
