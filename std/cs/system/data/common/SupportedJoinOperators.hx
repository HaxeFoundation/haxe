package cs.system.data.common;

/** Specifies what types of Transact-SQL join statements are supported by the data source. */
@:native("System.Data.Common.SupportedJoinOperators")
extern enum abstract SupportedJoinOperators(Int) {
	var FullOuter = 8;
	var Inner = 1;
	var LeftOuter = 2;
	var None = 0;
	var RightOuter = 4;
	@:op(A | B) static function or(lhs:SupportedJoinOperators, rhs:SupportedJoinOperators):SupportedJoinOperators;
	@:op(A & B) static function and(lhs:SupportedJoinOperators, rhs:SupportedJoinOperators):SupportedJoinOperators;
	@:op(A ^ B) static function xor(lhs:SupportedJoinOperators, rhs:SupportedJoinOperators):SupportedJoinOperators;
	@:op(~A) static function complement(value:SupportedJoinOperators):SupportedJoinOperators;
}
