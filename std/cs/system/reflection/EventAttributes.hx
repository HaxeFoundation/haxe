package cs.system.reflection;

/** Specifies the attributes of an event. */
@:native("System.Reflection.EventAttributes")
extern enum abstract EventAttributes(Int) {
	var None = 0;
	var ReservedMask = 1024;
	var RTSpecialName = 1024;
	var SpecialName = 512;
	@:op(A | B) static function or(lhs:EventAttributes, rhs:EventAttributes):EventAttributes;
	@:op(A & B) static function and(lhs:EventAttributes, rhs:EventAttributes):EventAttributes;
	@:op(A ^ B) static function xor(lhs:EventAttributes, rhs:EventAttributes):EventAttributes;
	@:op(~A) static function complement(value:EventAttributes):EventAttributes;
}
