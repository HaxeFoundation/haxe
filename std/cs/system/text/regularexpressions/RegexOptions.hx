package cs.system.text.regularexpressions;

/** Provides enumerated values to use to set regular expression options. */
@:native("System.Text.RegularExpressions.RegexOptions")
extern enum abstract RegexOptions(Int) {
	var Compiled = 8;
	var CultureInvariant = 512;
	var ECMAScript = 256;
	var ExplicitCapture = 4;
	var IgnoreCase = 1;
	var IgnorePatternWhitespace = 32;
	var Multiline = 2;
	var None = 0;
	var RightToLeft = 64;
	var Singleline = 16;
	@:op(A | B) static function or(lhs:RegexOptions, rhs:RegexOptions):RegexOptions;
	@:op(A & B) static function and(lhs:RegexOptions, rhs:RegexOptions):RegexOptions;
	@:op(A ^ B) static function xor(lhs:RegexOptions, rhs:RegexOptions):RegexOptions;
	@:op(~A) static function complement(value:RegexOptions):RegexOptions;
}
