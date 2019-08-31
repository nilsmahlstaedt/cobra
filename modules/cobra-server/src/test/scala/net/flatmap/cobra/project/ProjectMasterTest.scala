package net.flatmap.cobra.project

import org.scalatest.{FlatSpec, Matchers}

class ProjectMasterTest extends FlatSpec with Matchers {

  "The Path Parser" should "recognize everything enclosed within square brackets as keys" in {
    ProjectMaster.extractPathParts("[key] foobar") shouldBe Right(("key", "foobar"))
  }

  it should "recognize the path as everything after the key when separated by a space" in {
    ProjectMaster.extractPathParts("[path] path") shouldBe Right(("path", "path"))
    ProjectMaster.extractPathParts("[key]path") shouldBe Right(("key", "path"))
  }

  it should "not allow empty keys" in {
    ProjectMaster.extractPathParts("[] foobar") shouldBe a[Left[_, _]]
  }

  it should "not allow empty paths" in {
    ProjectMaster.extractPathParts("[key] ") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[key]") shouldBe a[Left[_, _]]
  }

  it should "only allow alpha-nums in key" in {
    ProjectMaster.extractPathParts("[käey908dajkjk3] foo") shouldBe Right(("käey908dajkjk3", "foo"))
    ProjectMaster.extractPathParts("[ ] foo") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[-] foo") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[_] foo") shouldBe a[Left[_, _]]
  }

  it should "allow everything except whitespace in path" in {
    ProjectMaster.extractPathParts("[key] foo") shouldBe Right(("key","foo"))
    ProjectMaster.extractPathParts("[key] foo.bar#baz") shouldBe Right(("key","foo.bar#baz"))
    ProjectMaster.extractPathParts("[key] foo-bar") shouldBe Right(("key","foo-bar"))
    ProjectMaster.extractPathParts("[key] foo_bar") shouldBe Right(("key","foo_bar"))
    ProjectMaster.extractPathParts("[key] foo?ß0bär") shouldBe Right(("key","foo?ß0bär"))

    ProjectMaster.extractPathParts("[key] foo bar baz") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[key] foo\tbarbaz") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[key] foo\nbarbaz") shouldBe a[Left[_, _]]
    ProjectMaster.extractPathParts("[key] foo\rbarbaz") shouldBe a[Left[_, _]]

  }
}
