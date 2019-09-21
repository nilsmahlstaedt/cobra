package net.flatmap.cobra.project

import net.flatmap.cobra.paths.Path
import org.scalatest.{FlatSpec, Matchers}

class PathSpec extends FlatSpec with Matchers {
  "Paths" should "be absolute, if they start with a single '/'" in {
    Path("/absolute").isAbsolute shouldBe true
    Path("relative").isAbsolute shouldBe false
    Path(" relative").isAbsolute shouldBe false
    Path("-relative").isAbsolute shouldBe false
    Path("\\relative").isAbsolute shouldBe false
  }
}
