package cs.system.runtime.versioning;

/** Describes the compatibility guarantee of a component, type, or type member that may span multiple versions. */
@:native("System.Runtime.Versioning.ComponentGuaranteesOptions")
extern enum abstract ComponentGuaranteesOptions(Int) {
	var Exchange = 1;
	var None = 0;
	var SideBySide = 4;
	var Stable = 2;
	@:op(A | B) static function or(lhs:ComponentGuaranteesOptions, rhs:ComponentGuaranteesOptions):ComponentGuaranteesOptions;
	@:op(A & B) static function and(lhs:ComponentGuaranteesOptions, rhs:ComponentGuaranteesOptions):ComponentGuaranteesOptions;
	@:op(A ^ B) static function xor(lhs:ComponentGuaranteesOptions, rhs:ComponentGuaranteesOptions):ComponentGuaranteesOptions;
	@:op(~A) static function complement(value:ComponentGuaranteesOptions):ComponentGuaranteesOptions;
}
