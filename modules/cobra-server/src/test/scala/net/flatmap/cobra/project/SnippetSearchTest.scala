package net.flatmap.cobra.project

import java.nio.file.Paths

import net.flatmap.cobra.Snippet
import net.flatmap.cobra.paths.{Path, SnippetSearch, TypeBound}
import org.eclipse.lsp4j.SymbolKind
import org.scalatest.{FlatSpec, FunSuite, Matchers}

class SnippetSearchTest extends FlatSpec with Matchers with SnippetSearch {
  val snippets: List[Snippet] = List(
    Snippet("myclass", Some("some/package/"), Paths.get("testfile"), 0, 0, SymbolKind.Class),
    Snippet("myclass.func1", Some("some/package/"), Paths.get("testfile"), 0, 0, SymbolKind.Class),
    Snippet("myclass.func1.foo", Some("some/package/"), Paths.get("testfile"), 0, 0, SymbolKind.Class),
    Snippet("myclass", Some("some/package/"), Paths.get("testfile"), 0, 0, SymbolKind.Object),
    Snippet("myfunc", Some("some/package/myclass/"), Paths.get("testfile"), 0, 0, SymbolKind.Function),
    Snippet("snippet", Some("some/other/"), Paths.get("testfile"), 0, 0, SymbolKind.Class),
  )

  "SnippetSearch" should "return all matches for an incomplete path" in {
    snippets.findSnippets(Path("myclass.func1")).head shouldBe snippets(1)
    snippets.findSnippets(Path("myclass")).head shouldBe snippets.head
    snippets.findSnippets(Path("foo")).head shouldBe snippets(2)

    snippets.findSnippets(Path("package")).length shouldBe 0
    snippets.findSnippets(Path("other")).length shouldBe 0
  }

  it should "find a complete path" in {
    val res = snippets.findSnippets(Path("some.package.myclass"))
    res.length shouldBe 2
    res shouldBe (snippets.head :: snippets(3) :: Nil)

    val res2 = snippets.findSnippets(Path("some.package.myclass.myfunc"))
    res2.length shouldBe 1
    res2 shouldBe List(snippets(4))


    snippets.findSnippets(Path("not.in.some.package")) shouldBe Nil
  }

  it should "find snippets with relativ paths" in {
    snippets.findSnippets(Path("package.myclass")) shouldBe
      snippets.findSnippets(Path("myclass"))
  }

  it should "treat a complete path the same no matter if it is absolute or not" in {
    snippets.findSnippets(Path("some.package.myclass")) shouldBe
      snippets.findSnippets(Path("package.myclass"))

    snippets.findSnippets(Path("some.package")) shouldBe
      snippets.findSnippets(Path("package"))
  }

  it should "respect type bounds" in {
    val res = snippets.findSnippets(Path("some.package.myclass", TypeBound(SymbolKind.Class)))
    res.length shouldBe 1
    res.head shouldBe snippets(0)

    val res2 = snippets.findSnippets(Path("some.package.myclass", TypeBound(SymbolKind.Object)))
    res2.length shouldBe 1
    res2.head shouldBe snippets(3)
  }

  it should "not find nodes where the name is only given partially" in {
    snippets.findSnippets(Path("some.pack")) shouldBe Nil
    snippets.findSnippets(Path("class")) shouldBe Nil
  }

}
