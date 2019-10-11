package net.flatmap.cobra.project

import java.nio.file.Paths

import net.flatmap.cobra.Snippet
import net.flatmap.cobra.paths.{Path, SnippetSearch, TypeBound}
import org.eclipse.lsp4j.SymbolKind
import org.scalatest.{FlatSpec, FunSuite, Matchers}

class SnippetSearchTest extends FlatSpec with Matchers with SnippetSearch {
  val snippets: List[Snippet] = List(
    Snippet("myclass", Some("some/package/"), Paths.get("testfile"), 0, 0, SymbolKind.Class),
    Snippet("myclass", Some("some/package/"), Paths.get("testfile"), 0, 0, SymbolKind.Object),
    Snippet("myfunc", Some("some/package/myclass/"), Paths.get("testfile"), 0, 0, SymbolKind.Function),
    Snippet("snippet", Some("some/other/"), Paths.get("testfile"), 0, 0, SymbolKind.Class),
  )

  "SnippetSearch" should "return all matches for an incomplete absolute path" in {
    val p = Path("/some/package/")
    val res = snippets.findSnippets(p)
    res.length shouldBe 3
    res shouldBe snippets.take(3)
  }

  it should "return all matches for an incomplete relative path" in {
    val res = snippets.findSnippets(Path("package"))
    res.length shouldBe 3
    res shouldBe snippets.take(3)

    val res2 = snippets.findSnippets(Path("other"))
    res2.length shouldBe 1
    res2 shouldBe List(snippets(3))
  }

  it should "find a complete path" in {
    val res = snippets.findSnippets(Path("/some/package/myclass"))
    res.length shouldBe 3
    res shouldBe snippets.take(3)

    val res2 = snippets.findSnippets(Path("/some/package/myclass/myfunc"))
    res2.length shouldBe 1
    res2 shouldBe List(snippets(2))


    val res3 = snippets.findSnippets(Path("/no/in/some/package"))
    res3 shouldBe Nil
  }

  it should "find snippets with relativ paths" in {
    snippets.findSnippets(Path("package/myclass")) shouldBe
      snippets.findSnippets(Path("myclass"))
  }

  it should "treat a complete path the same no matter if it is absolute or not" in {
    snippets.findSnippets(Path("some/package/myclass")) shouldBe
      snippets.findSnippets(Path("/some/package/myclass"))

    snippets.findSnippets(Path("some/package")) shouldBe
      snippets.findSnippets(Path("/some/package"))
  }

  it should "respect type bounds" in {
    val res = snippets.findSnippets(Path("some/package", TypeBound(SymbolKind.Class)))
    res.length shouldBe 1
    res.head shouldBe snippets(0)

    val res2 = snippets.findSnippets(Path("some/package", TypeBound(SymbolKind.Object)))
    res2.length shouldBe 1
    res2.head shouldBe snippets(1)
  }

  it should "not find nodes where the name is only given partially" in {
    snippets.findSnippets(Path("/some/pack")) shouldBe Nil
    snippets.findSnippets(Path("class")) shouldBe Nil
  }

}
