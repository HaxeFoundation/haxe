package cs.system.reflection;

/** Defines the valid calling conventions for a method. */
@:native("System.Reflection.CallingConventions")
extern enum abstract CallingConventions(Int) {
	var Any = 3;
	var ExplicitThis = 64;
	var HasThis = 32;
	var Standard = 1;
	var VarArgs = 2;
	@:op(A | B) static function or(lhs:CallingConventions, rhs:CallingConventions):CallingConventions;
	@:op(A & B) static function and(lhs:CallingConventions, rhs:CallingConventions):CallingConventions;
	@:op(A ^ B) static function xor(lhs:CallingConventions, rhs:CallingConventions):CallingConventions;
	@:op(~A) static function complement(value:CallingConventions):CallingConventions;
}
