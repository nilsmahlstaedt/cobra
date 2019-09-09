package net.flatmap.cobra.project

import net.flatmap.cobra.project.ProjectMaster.{Path, ProjectAssociation, TypeBound}
import org.eclipse.lsp4j.SymbolKind
import org.scalatest.{FlatSpec, Matchers}

class ProjectMasterTest extends FlatSpec with Matchers {

  "The Path Parser" should "recognize everything enclosed within square brackets as keys" in {
    ProjectMaster.extractPathParts("[p:key] foobar") shouldBe Right(Path("foobar", ProjectAssociation("key")))
    ProjectMaster.extractPathParts("[p:key] foobar") shouldBe Right(Path("foobar.bar.baz", ProjectAssociation("key")))
    ProjectMaster.extractPathParts("[t:function] foobar") shouldBe Right(Path("foobar", TypeBound(SymbolKind.Function)))
    ProjectMaster.extractPathParts("[p:key] [t:Function] foobar") shouldBe Right(Path("foobar", ProjectAssociation("key"), TypeBound(SymbolKind.Function)))
    ProjectMaster.extractPathParts("[t:Function] [p:key] foobar") shouldBe Right(Path("foobar", ProjectAssociation("key"), TypeBound(SymbolKind.Function)))
    ProjectMaster.extractPathParts("[t:Function][p:key] foobar") shouldBe Right(Path("foobar", ProjectAssociation("key"), TypeBound(SymbolKind.Function)))
  }

  it should "recognize the path as everything after the key when separated by a space" in {
    ProjectMaster.extractPathParts("[p:path] path") shouldBe Right(Path("path", ProjectAssociation("path")))
    ProjectMaster.extractPathParts("[p:key]path") shouldBe Right(Path("path", ProjectAssociation("key")))
    ProjectMaster.extractPathParts("[p:key] some.path") shouldBe Right(Path("some.path", ProjectAssociation("key")))
  }

  it should "not allow empty keys" in {
    ProjectMaster.extractPathParts("[] foobar") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[p:] foobar") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[t:] foobar") shouldBe a[Left[_, _]]
  }

  it should "not allow keys without tag" in {
    ProjectMaster.extractPathParts("[key] foobar") shouldBe a[Left[_, _]]
  }

  it should "not allow keys without assigned tag" in {
    ProjectMaster.extractPathParts("[key:key] foobar") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[k:key] foobar") shouldBe a[Left[_, _]]
  }

  it should "not allow empty paths" in {
    ProjectMaster.extractPathParts("[p:key] ") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[t:key] ") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[p:key]") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[t:key]") shouldBe a[Left[_, _]]
  }

  it should "only allow alpha-nums in project key" in {
    ProjectMaster.extractPathParts("[p:käey908dajkjk3] foo") shouldBe Right(Path("foo",ProjectAssociation("käey908dajkjk3")))
    ProjectMaster.extractPathParts("[p: ] foo") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[p:-] foo") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[p:_] foo") shouldBe a[Left[_, _]]
  }

  it should "allow everything except whitespace in path" in {
    ProjectMaster.extractPathParts("[p:key] foo") shouldBe Right(Path("foo", ProjectAssociation("key")))
    ProjectMaster.extractPathParts("[p:key] foo.bar#baz") shouldBe Right(Path("foo.bar#baz", ProjectAssociation("key")))
    ProjectMaster.extractPathParts("[p:key] foo-bar") shouldBe Right(Path("foo-bar", ProjectAssociation("key")))
    ProjectMaster.extractPathParts("[p:key] foo_bar") shouldBe Right(Path("foo_bar", ProjectAssociation("key")))
    ProjectMaster.extractPathParts("[p:key] foo?ß0bär") shouldBe Right(Path("foo?ß0bär", ProjectAssociation("key")))

    ProjectMaster.extractPathParts("[p:key] foo bar baz") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[p:key] foo\tbarbaz") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[p:key] foo\nbarbaz") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[p:key] foo\rbarbaz") shouldBe a[Left[_, _]]
  }
}
