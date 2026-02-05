package cs.system.data.sqltypes;

/** Specifies the compare option values for a  structure. */
@:native("System.Data.SqlTypes.SqlCompareOptions")
extern enum abstract SqlCompareOptions(Int) {
	var BinarySort = 32768;
	var BinarySort2 = 16384;
	var IgnoreCase = 1;
	var IgnoreKanaType = 8;
	var IgnoreNonSpace = 2;
	var IgnoreWidth = 16;
	var None = 0;
	@:op(A | B) static function or(lhs:SqlCompareOptions, rhs:SqlCompareOptions):SqlCompareOptions;
	@:op(A & B) static function and(lhs:SqlCompareOptions, rhs:SqlCompareOptions):SqlCompareOptions;
	@:op(A ^ B) static function xor(lhs:SqlCompareOptions, rhs:SqlCompareOptions):SqlCompareOptions;
	@:op(~A) static function complement(value:SqlCompareOptions):SqlCompareOptions;
}
