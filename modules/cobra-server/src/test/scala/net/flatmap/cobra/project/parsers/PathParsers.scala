package net.flatmap.cobra.project.parsers

import net.flatmap.cobra.paths.{Path, PathParser, ProjectAssociation, TypeBound}
import org.eclipse.lsp4j.SymbolKind
import org.scalatest.{FlatSpec, Inside, Matchers}

/**
 *
 */
class PathParsers extends FlatSpec with Matchers with Inside with PathParser {

  "The Path Parser" should "recognize paths" in {
    extractPathParts("foobar") shouldBe Right(Path("foobar", Nil))
    extractPathParts("/foobar") shouldBe Right(Path("foobar", Nil))
    extractPathParts("foo/bar/baz") shouldBe Right(Path("foo.bar.baz", Nil))
    extractPathParts("/foo/bar/baz") shouldBe Right(Path("foo.bar.baz", Nil))
    extractPathParts("  foo/bar/baz") shouldBe Right(Path("foo.bar.baz", Nil))
    extractPathParts("  /foo/bar/baz") shouldBe Right(Path("foo.bar.baz", Nil))
    extractPathParts("foo/bar/baz  ") shouldBe Right(Path("foo.bar.baz", Nil))
    extractPathParts("/foo/bar/baz  ") shouldBe Right(Path("foo.bar.baz", Nil))
    extractPathParts("  foo/bar/baz  ") shouldBe Right(Path("foo.bar.baz", Nil))
    extractPathParts("  /foo/bar/baz  ") shouldBe Right(Path("foo.bar.baz", Nil))
    extractPathParts("  foo.bar.baz  ") shouldBe Right(Path("foo.bar.baz", Nil))
  }

  it should "recognize everything enclosed within square brackets as keys" in {
    extractPathParts("[p:key] foobar1") shouldBe Right(Path("foobar1", ProjectAssociation("key")))
    extractPathParts("[p:key] foobar/bar/baz1") shouldBe Right(Path("foobar.bar.baz1", ProjectAssociation("key")))
    extractPathParts("[t:function] foobar2") shouldBe Right(Path("foobar2", TypeBound(SymbolKind.Function)))
    extractPathParts("[t:function] /foobar/bar/baz2") shouldBe Right(Path("foobar.bar.baz2", TypeBound(SymbolKind.Function)))
  }

  it should "recognize multiple keys" in {
    extractPathParts("[p:key] [t:Function] foobar1") shouldBe Right(Path("foobar1", ProjectAssociation("key"), TypeBound(SymbolKind.Function)))
    extractPathParts("[t:Function] [p:key] foobar2") shouldBe Right(Path("foobar2", TypeBound(SymbolKind.Function), ProjectAssociation("key")))
    extractPathParts("[t:Function][p:key] foobar3") shouldBe Right(Path("foobar3", TypeBound(SymbolKind.Function), ProjectAssociation("key")))
  }

  it should "recognize the path as everything after the key when separated by a space" in {
    extractPathParts("[p:path] path") shouldBe Right(Path("path", ProjectAssociation("path")))
    extractPathParts("[p:key]path") shouldBe Right(Path("path", ProjectAssociation("key")))
    extractPathParts("[p:key] some/path") shouldBe Right(Path("some.path", ProjectAssociation("key")))
  }

  it should "discard empty keys" in {
    extractPathParts("[] foo") shouldBe Right(Path("foo"))
    extractPathParts("[][] foo") shouldBe Right(Path("foo"))
    extractPathParts("[] [] foo") shouldBe Right(Path("foo"))
  }

  it should "discard incomplete keys" in {
    extractPathParts("[p:] foo") shouldBe Right(Path("foo"))
    extractPathParts("[t:] foo") shouldBe Right(Path("foo"))
    extractPathParts("[p:][t:] foo") shouldBe Right(Path("foo"))
    extractPathParts("[t:][p:] foo") shouldBe Right(Path("foo"))
  }

  it should "discard unkown keys" in {
    extractPathParts("[x:value] foo") shouldBe Right(Path("foo"))
    extractPathParts("[y:key] foo") shouldBe Right(Path("foo"))
    extractPathParts("[z:key1][l:key2] foo") shouldBe Right(Path("foo"))
    extractPathParts("[ü:key3][ö:key4] foo") shouldBe Right(Path("foo"))
  }

  it should "allow whitespace between defining parts in keys" in {
    extractPathParts("[p:path] path") shouldBe Right(Path("path", ProjectAssociation("path")))
    extractPathParts("[  p    :path] path") shouldBe Right(Path("path", ProjectAssociation("path")))
    extractPathParts("[  p :path] path") shouldBe Right(Path("path", ProjectAssociation("path")))
  }

  it should "not allow keys without tag" in {
    extractPathParts("[key] foobar") shouldBe Right(Path("foobar", Nil))
  }

  it should "not allow keys without assigned tag" in {
    extractPathParts("[key:key] foobar") shouldBe Right(Path("foobar", Nil))
    extractPathParts("[k:key] foobar") shouldBe Right(Path("foobar", Nil))
  }

  it should "not allow empty paths" in {
    extractPathParts("[p:key] ") shouldBe a[Left[_, _]]
    extractPathParts("[t:key] ") shouldBe a[Left[_, _]]
    extractPathParts("[p:key]") shouldBe a[Left[_, _]]
    extractPathParts("[t:key]") shouldBe a[Left[_, _]]
  }

  it should "allow allow only specified seperators in path" in {
    extractPathParts("[p:key] foo") shouldBe Right(Path("foo", ProjectAssociation("key")))
    extractPathParts("[p:key] foo/bar#baz") shouldBe Right(Path("foo.bar.baz", ProjectAssociation("key")))
    extractPathParts("[p:key] foo-bar") shouldBe a[Left[_, _]]
    extractPathParts("[p:key] foo_bar") shouldBe a[Left[_, _]]
    extractPathParts("[p:key] foo?ß0bär") shouldBe a[Left[_, _]]

    extractPathParts("[p:key] foo bar baz") shouldBe a[Left[_, _]]
    extractPathParts("[p:key] foo\tbarbaz") shouldBe a[Left[_, _]]
    extractPathParts("[p:key] foo\nbarbaz") shouldBe a[Left[_, _]]
    extractPathParts("[p:key] foo\rbarbaz") shouldBe a[Left[_, _]]
  }

  it should "not allow double seperators" in {
    extractPathParts("[p:key] foo##bar") shouldBe a[Left[_, _]]
    extractPathParts("[p:key] foo..bar") shouldBe a[Left[_, _]]
    extractPathParts("[p:key] foo//bar") shouldBe a[Left[_, _]]
    extractPathParts("[p:key] ..foo//bar") shouldBe a[Left[_, _]]
    extractPathParts("..foo//bar") shouldBe a[Left[_, _]]
  }
}
