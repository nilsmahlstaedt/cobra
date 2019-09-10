package net.flatmap.cobra.project

import fastparse.Parsed.{Failure, Success}
import net.flatmap.cobra.paths.{Path, PathParser, ProjectAssociation, TypeBound}
import org.eclipse.lsp4j.SymbolKind
import org.scalatest.{FlatSpec, Inside, Matchers}
import fastparse._
import net.flatmap.cobra.paths.PathParser._

class ProjectMasterTest extends FlatSpec with Matchers with Inside with PathParser {

  "The Path Parser" should "recognize everything enclosed within square brackets as keys" in {

    extractPathParts("[p:key] foobar") shouldBe Right(Path("foobar", ProjectAssociation("key")))
    extractPathParts("[p:key] foobar") shouldBe Right(Path("foobar.bar.baz", ProjectAssociation("key")))
    extractPathParts("[t:function] foobar") shouldBe Right(Path("foobar", TypeBound(SymbolKind.Function)))
    extractPathParts("[p:key] [t:Function] foobar") shouldBe Right(Path("foobar", ProjectAssociation("key"), TypeBound(SymbolKind.Function)))
    extractPathParts("[t:Function] [p:key] foobar") shouldBe Right(Path("foobar", ProjectAssociation("key"), TypeBound(SymbolKind.Function)))
    extractPathParts("[t:Function][p:key] foobar") shouldBe Right(Path("foobar", ProjectAssociation("key"), TypeBound(SymbolKind.Function)))
  }

  it should "recognize the path as everything after the key when separated by a space" in {
    extractPathParts("[p:path] path") shouldBe Right(Path("path", ProjectAssociation("path")))
    extractPathParts("[p:key]path") shouldBe Right(Path("path", ProjectAssociation("key")))
    extractPathParts("[p:key] some.path") shouldBe Right(Path("some.path", ProjectAssociation("key")))
  }

  it should "not allow empty keys" in {
    extractPathParts("[] foo") shouldBe Right(Path("foo"))
    extractPathParts("[][] foo") shouldBe Right(Path("foo"))
    extractPathParts("[] [] foo") shouldBe Right(Path("foo"))
    extractPathParts("[p:] foo") shouldBe Right(Path("foo"))
    extractPathParts("[t:] foo") shouldBe Right(Path("foo"))
    extractPathParts("[p:][t:] foo") shouldBe Right(Path("foo"))
    extractPathParts("[t:][p:] foo") shouldBe Right(Path("foo"))
  }

  it should "accept correctly formatted type bounds" in {
    inside(parse("file", typeBound(_))) {
      case Success(TypeBound(t), _) => t shouldBe SymbolKind.File
    }

    inside(parse("File", typeBound(_))) {
      case Success(TypeBound(t), _) => t shouldBe SymbolKind.File
    }

    inside(parse("FILE", typeBound(_))) {
      case Success(TypeBound(t), _) => t shouldBe SymbolKind.File
    }

    inside(parse("EnumMember", typeBound(_))) {
      case Success(TypeBound(t), _) => t shouldBe SymbolKind.EnumMember
    }

    inside(parse("enummember", typeBound(_))) {
      case Success(TypeBound(t), _) => t shouldBe SymbolKind.EnumMember
    }

    parse("foobar", typeBound(_)) shouldBe a[Failure]
    parse("EnumMmbr", typeBound(_)) shouldBe a[Failure]
  }

  it should "allow whitespace between defining parts in keys" in {
    extractPathParts("[p:path] path") shouldBe Right(Path("path", ProjectAssociation("path")))
    extractPathParts("[  p    :path] path") shouldBe Right(Path("path", ProjectAssociation("path")))
    extractPathParts("[  p :path] path") shouldBe Right(Path("path", ProjectAssociation("path")))
  }

  it should "not allow duplicate keys" in {
    extractPathParts("[p:key] path") shouldBe Right(Path("path", ProjectAssociation("key")))
    extractPathParts("[f:file] path") shouldBe Right(Path("path", TypeBound(SymbolKind.File)))
    extractPathParts("[p:key][p:key] foobar") shouldBe a[Left[_, _]]
    extractPathParts("[t:File][t:file] foobar") shouldBe a[Left[_, _]]
  }

  it should "only allow type bounds defined in the LSP spec" in {
    extractPathParts("[t:File] path") shouldBe Right(Path("path", TypeBound(SymbolKind.File)))
    extractPathParts("[t:file] path") shouldBe Right(Path("path", TypeBound(SymbolKind.File)))
    extractPathParts("[t:FILE] path") shouldBe Right(Path("path", TypeBound(SymbolKind.File)))
    extractPathParts("[t:EnumMember] path") shouldBe Right(Path("path", TypeBound(SymbolKind.EnumMember)))

    extractPathParts("[t:Enummember] path") shouldBe a[Left[_, _]]
    extractPathParts("[t:enumMember] path") shouldBe a[Left[_, _]]
  }

  it should "not allow keys without tag" in {
    extractPathParts("[key] foobar") shouldBe a[Left[_, _]]
  }

  it should "not allow keys without assigned tag" in {
    extractPathParts("[key:key] foobar") shouldBe a[Left[_, _]]
    extractPathParts("[k:key] foobar") shouldBe a[Left[_, _]]
  }

  it should "not allow empty paths" in {
    extractPathParts("[p:key] ") shouldBe a[Left[_, _]]
    extractPathParts("[t:key] ") shouldBe a[Left[_, _]]
    extractPathParts("[p:key]") shouldBe a[Left[_, _]]
    extractPathParts("[t:key]") shouldBe a[Left[_, _]]
  }

  it should "only allow alpha-nums in project key" in {
    extractPathParts("[p:käey908dajkjk3] foo") shouldBe Right(Path("foo",ProjectAssociation("käey908dajkjk3")))
    extractPathParts("[p: ] foo") shouldBe a[Left[_, _]]
    extractPathParts("[p:-] foo") shouldBe a[Left[_, _]]
    extractPathParts("[p:_] foo") shouldBe a[Left[_, _]]
  }

  it should "allow everything except whitespace in path" in {
    extractPathParts("[p:key] foo") shouldBe Right(Path("foo", ProjectAssociation("key")))
    extractPathParts("[p:key] foo.bar#baz") shouldBe Right(Path("foo.bar#baz", ProjectAssociation("key")))
    extractPathParts("[p:key] foo-bar") shouldBe Right(Path("foo-bar", ProjectAssociation("key")))
    extractPathParts("[p:key] foo_bar") shouldBe Right(Path("foo_bar", ProjectAssociation("key")))
    extractPathParts("[p:key] foo?ß0bär") shouldBe Right(Path("foo?ß0bär", ProjectAssociation("key")))

    extractPathParts("[p:key] foo bar baz") shouldBe a[Left[_, _]]
    extractPathParts("[p:key] foo\tbarbaz") shouldBe a[Left[_, _]]
    extractPathParts("[p:key] foo\nbarbaz") shouldBe a[Left[_, _]]
    extractPathParts("[p:key] foo\rbarbaz") shouldBe a[Left[_, _]]
  }
}
