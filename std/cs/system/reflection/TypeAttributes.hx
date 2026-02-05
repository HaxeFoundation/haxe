package cs.system.reflection;

/** Specifies type attributes. */
@:native("System.Reflection.TypeAttributes")
extern enum abstract TypeAttributes(Int) {
	var Abstract = 128;
	var AnsiClass = 0;
	var AutoClass = 131072;
	var AutoLayout = 0;
	var BeforeFieldInit = 1048576;
	var Class = 0;
	var ClassSemanticsMask = 32;
	var CustomFormatClass = 196608;
	var CustomFormatMask = 12582912;
	var ExplicitLayout = 16;
	var HasSecurity = 262144;
	var Import = 4096;
	var Interface = 32;
	var LayoutMask = 24;
	var NestedAssembly = 5;
	var NestedFamANDAssem = 6;
	var NestedFamily = 4;
	var NestedFamORAssem = 7;
	var NestedPrivate = 3;
	var NestedPublic = 2;
	var NotPublic = 0;
	var Public = 1;
	var ReservedMask = 264192;
	var RTSpecialName = 2048;
	var Sealed = 256;
	var SequentialLayout = 8;
	var Serializable = 8192;
	var SpecialName = 1024;
	var StringFormatMask = 196608;
	var UnicodeClass = 65536;
	var VisibilityMask = 7;
	var WindowsRuntime = 16384;
	@:op(A | B) static function or(lhs:TypeAttributes, rhs:TypeAttributes):TypeAttributes;
	@:op(A & B) static function and(lhs:TypeAttributes, rhs:TypeAttributes):TypeAttributes;
	@:op(A ^ B) static function xor(lhs:TypeAttributes, rhs:TypeAttributes):TypeAttributes;
	@:op(~A) static function complement(value:TypeAttributes):TypeAttributes;
}
