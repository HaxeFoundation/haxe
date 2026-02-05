package cs.system.runtime.versioning;

/** Identifies the scope of a sharable resource. */
@:native("System.Runtime.Versioning.ResourceScope")
extern enum abstract ResourceScope(Int) {
	var AppDomain = 4;
	var Assembly = 32;
	var Library = 8;
	var Machine = 1;
	var None = 0;
	var Private = 16;
	var Process = 2;
	@:op(A | B) static function or(lhs:ResourceScope, rhs:ResourceScope):ResourceScope;
	@:op(A & B) static function and(lhs:ResourceScope, rhs:ResourceScope):ResourceScope;
	@:op(A ^ B) static function xor(lhs:ResourceScope, rhs:ResourceScope):ResourceScope;
	@:op(~A) static function complement(value:ResourceScope):ResourceScope;
}
