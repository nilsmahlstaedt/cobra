package net.flatmap.cobra.paths

import fastparse._

/**
 * string values of symbol type names as defined in
 * https://microsoft.github.io/language-server-protocol/specification
 */
object TypeNames {
  /*
  from https://microsoft.github.io/language-server-protocol/specification (SymbolKind)

  export namespace SymbolKind {
	export const File = 1;
	export const Module = 2;
	export const Namespace = 3;
	export const Package = 4;
	export const Class = 5;
	export const Method = 6;
	export const Property = 7;
	export const Field = 8;
	export const Constructor = 9;
	export const Enum = 10;
	export const Interface = 11;
	export const Function = 12;
	export const Variable = 13;
	export const Constant = 14;
	export const String = 15;
	export const Number = 16;
	export const Boolean = 17;
	export const Array = 18;
	export const Object = 19;
	export const Key = 20;
	export const Null = 21;
	export const EnumMember = 22;
	export const Struct = 23;
	export const Event = 24;
	export const Operator = 25;
	export const TypeParameter = 26;
}
   */

  def namesParser[_: P]: P[String] = {
    import NoWhitespace._
    // P(StringInIgnoreCase(names:_*) sadly does not work in this case as it fails to compile with:
    // Function can only accept constant singleton type

    P(StringInIgnoreCase(
      "File",
      "Module",
      "Namespace",
      "Package",
      "Class",
      "Method",
      "Property",
      "Field",
      "Constructor",
      "Enum",
      "Interface",
      "Function",
      "Variable",
      "Constant",
      "String",
      "Number",
      "Boolean",
      "Array",
      "Object",
      "Key",
      "Null",
      "EnumMember",
      "Struct",
      "Event",
      "Operator",
      "TypeParameter"
    ).! ~ &(CharPred(!_.isLetter) | End)).map(recapitalize).filter(_.isDefined).map(_.get)
  }

  val names = Set(
    "File",
    "Module",
    "Namespace",
    "Package",
    "Class",
    "Method",
    "Property",
    "Field",
    "Constructor",
    "Enum",
    "Interface",
    "Function",
    "Variable",
    "Constant",
    "String",
    "Number",
    "Boolean",
    "Array",
    "Object",
    "Key",
    "Null",
    "EnumMember",
    "Struct",
    "Event",
    "Operator",
    "TypeParameter"
  )

  val recapitalize: String => Option[String] = {
    names.map(n => n.toLowerCase -> n).toMap
    }.lift.compose((s: String) => s.toLowerCase)

  //val singleWords = names.filter(s => s.headOption.exists(_.isUpper) && s.tail.forall(_.isLower))
  //val multiWords = names.diff(singleWords)
}
