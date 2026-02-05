package cs.system.globalization;

/** Defines the string comparison options to use with . */
@:native("System.Globalization.CompareOptions")
extern enum abstract CompareOptions(Int) {
	var IgnoreCase = 1;
	var IgnoreKanaType = 8;
	var IgnoreNonSpace = 2;
	var IgnoreSymbols = 4;
	var IgnoreWidth = 16;
	var None = 0;
	var Ordinal = 1073741824;
	var OrdinalIgnoreCase = 268435456;
	var StringSort = 536870912;
	@:op(A | B) static function or(lhs:CompareOptions, rhs:CompareOptions):CompareOptions;
	@:op(A & B) static function and(lhs:CompareOptions, rhs:CompareOptions):CompareOptions;
	@:op(A ^ B) static function xor(lhs:CompareOptions, rhs:CompareOptions):CompareOptions;
	@:op(~A) static function complement(value:CompareOptions):CompareOptions;
}
