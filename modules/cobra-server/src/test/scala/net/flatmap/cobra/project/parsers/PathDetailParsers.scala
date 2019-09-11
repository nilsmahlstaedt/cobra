package net.flatmap.cobra.project.parsers

import fastparse.Parsed.{Failure, Success}
import fastparse.parse
import net.flatmap.cobra.paths.{PathParser, ProjectAssociation, TypeBound}
import org.eclipse.lsp4j.SymbolKind
import org.scalatest.{FlatSpec, Matchers}

class PathDetailParsers extends FlatSpec with Matchers with PathParser {

  "The TypeBound Parser" should "accept a value if it is defined as a symbolkind in the LSP spec" in {
    parse("File", typeBound(_)) shouldBe Success(TypeBound(SymbolKind.File), 4)
    parse("EnumMember", typeBound(_)) shouldBe Success(TypeBound(SymbolKind.EnumMember), 10)
  }

  it should "accept value disregarding their exact case" in {
    parse("file", typeBound(_)) shouldBe Success(TypeBound(SymbolKind.File), 4)
    parse("fIlE", typeBound(_)) shouldBe Success(TypeBound(SymbolKind.File), 4)
    parse("enummember", typeBound(_)) shouldBe Success(TypeBound(SymbolKind.EnumMember), 10)
    parse("eNuMmEmBeR", typeBound(_)) shouldBe Success(TypeBound(SymbolKind.EnumMember), 10)
  }

  it should "not accept values not defined in the spec" in {
    parse("Fnctn", typeBound(_)) shouldBe a[Failure]
    parse("EnumMmbr", typeBound(_)) shouldBe a[Failure]
    parse("FileFunctionTool", typeBound(_)) shouldBe a[Failure]
    parse("EnumX", typeBound(_)) shouldBe a[Failure]
    parse("nummember", typeBound(_)) shouldBe a[Failure]
  }

  it should "accept a value in key format whith tag 't'" in {
    parse("[t:file]",typeKey(_)) shouldBe Success(TypeBound(SymbolKind.File), 8)
    parse("[t:enummember]",typeKey(_)) shouldBe Success(TypeBound(SymbolKind.EnumMember), 14)
  }

  it should "not accept keys with other tags" in {
    parse("[t2:enummember]",typeKey(_)) shouldBe a[Failure]
    parse("[s:enummember]",typeKey(_)) shouldBe a[Failure]
    parse("[:enummember]",typeKey(_)) shouldBe a[Failure]
  }

  "The ProjectAssoc Parser" should "accept values that are alphaNum" in {
    parse("foo", projectAssociation(_)) shouldBe Success(ProjectAssociation("foo"), 3)
    parse("foobarbaz", projectAssociation(_)) shouldBe Success(ProjectAssociation("foobarbaz"), 9)
    parse("1337leet", projectAssociation(_)) shouldBe Success(ProjectAssociation("1337leet"), 8)
  }

  it should "only accept input up to the first non alphaNum Char" in {
    parse("foo bar", projectAssociation(_)) shouldBe Success(ProjectAssociation("foo"), 3)
    parse("foo.bar", projectAssociation(_)) shouldBe Success(ProjectAssociation("foo"), 3)
  }

  it should "require at least one char" in {
    parse("", projectAssociation(_)) shouldBe a[Failure]
    parse(".a", projectAssociation(_)) shouldBe a[Failure]
    parse("a", projectAssociation(_)) shouldBe a[Success[_]]
  }
}
