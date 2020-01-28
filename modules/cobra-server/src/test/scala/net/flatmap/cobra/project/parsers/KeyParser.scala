package net.flatmap.cobra.project.parsers

import fastparse.Parsed.{Failure, Success}
import fastparse._
import net.flatmap.cobra.paths.{PathParser, ProjectAssociation, TypeBound}
import org.eclipse.lsp4j.SymbolKind
import org.scalatest.{FlatSpec, Matchers}

/**
 * tests the robustness of the key parser generation function
 */
class KeyParser extends FlatSpec with Matchers with PathParser {

  private object Parsers {
    import fastparse._
    import SingleLineWhitespace._

    def inner[_:P]: P[String] = P(CharsWhileIn("abc", 1).!)
    def p[_:P]: P[String] = KeyParser.this.key("c", inner)
    def ps[_:P]: P[Seq[String]] = (p.rep(1))


  }

  "The Key Constructor" should "require at least on a,b or c as inner value" in {
    parse("aaa", Parsers.inner(_)) shouldBe Success("aaa", 3)
    parse("abc", Parsers.inner(_)) shouldBe Success("abc", 3)
    parse("e", Parsers.inner(_)) shouldBe a[Failure]
    parse("", Parsers.inner(_)) shouldBe a[Failure]
  }

  it should "require square brackets and the tag char c for keys" in {
    parse("[c:abc]", Parsers.p(_)) shouldBe Success("abc", 7)
    parse("[c:a]", Parsers.p(_)) shouldBe Success("a", 5)
    parse("[:abc]", Parsers.p(_)) shouldBe a[Failure]
    parse("[e:abc]", Parsers.p(_)) shouldBe a[Failure]
    parse("[c:]", Parsers.p(_)) shouldBe a[Failure]
    parse("[c:e]", Parsers.p(_)) shouldBe a[Failure]
    parse("[]", Parsers.p(_)) shouldBe a[Failure]
  }

  it should "accept more than one properly formatted key" in {
    parse("[c:a] [c:b]", Parsers.ps(_)) shouldBe Success(Seq("a", "b"), 11)
    parse("[c:a][c:b]", Parsers.ps(_)) shouldBe Success(Seq("a", "b"), 10)
    parse("[c:a][e:b]", Parsers.ps(_)) shouldBe Success(Seq("a"), 5)
    parse("[][c:b]", Parsers.ps(_)) shouldBe a[Failure]
  }

  "The Keys Parser" should "single keys" in {
    parse("[t:File]", keys(_)) shouldBe Success(List(TypeBound(SymbolKind.File)), 8)
    parse("[p:project]", keys(_)) shouldBe Success(List(ProjectAssociation("project")), 11)
  }

  it should "parse multiple keys" in {
    parse("[t:File] [t:Function]", keys(_)) shouldBe Success(List(TypeBound(SymbolKind.File)), 21)
    parse("[t:File] [p:project]", keys(_)) shouldBe Success(List(TypeBound(SymbolKind.File), ProjectAssociation("project")), 20)
    parse("[p:project] [t:File]", keys(_)) shouldBe Success(List(ProjectAssociation("project"), TypeBound(SymbolKind.File)), 20)
    parse("[p:project1] [p:project2]", keys(_)) shouldBe Success(List(ProjectAssociation("project1")), 25)
  }

  it should "discard unkown or broken intermiexed keys" in {
    parse("[][t:File] [t:Function]", keys(_)) shouldBe Success(List(TypeBound(SymbolKind.File)), 23)
    parse("[x:][t:File] [t:Function]", keys(_)) shouldBe Success(List(TypeBound(SymbolKind.File)), 25)
    parse("[t:][t:File] [t:Function]", keys(_)) shouldBe Success(List(TypeBound(SymbolKind.File)), 25)
    parse("[t:][t:File] [] [p:project] []", keys(_)) shouldBe Success(List(TypeBound(SymbolKind.File), ProjectAssociation("project")), 30)
  }

  it should "accept duplicate keys and only return the first" in {
    parse("[p:key]", keys(_)).get.value shouldBe List(ProjectAssociation("key"))
    parse("[t:file]", keys(_)).get.value shouldBe List(TypeBound(SymbolKind.File))
    parse("[p:key1][p:key2]", keys(_)).get.value shouldBe List(ProjectAssociation("key1"))
    parse("[t:File][t:Function]", keys(_)).get.value shouldBe List(TypeBound(SymbolKind.File))
  }
}
