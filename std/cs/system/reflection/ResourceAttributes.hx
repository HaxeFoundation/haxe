package cs.system.reflection;

/** Specifies the attributes for a manifest resource. */
@:native("System.Reflection.ResourceAttributes")
extern enum abstract ResourceAttributes(Int) {
	var Private = 2;
	var Public = 1;
	@:op(A | B) static function or(lhs:ResourceAttributes, rhs:ResourceAttributes):ResourceAttributes;
	@:op(A & B) static function and(lhs:ResourceAttributes, rhs:ResourceAttributes):ResourceAttributes;
	@:op(A ^ B) static function xor(lhs:ResourceAttributes, rhs:ResourceAttributes):ResourceAttributes;
	@:op(~A) static function complement(value:ResourceAttributes):ResourceAttributes;
}
