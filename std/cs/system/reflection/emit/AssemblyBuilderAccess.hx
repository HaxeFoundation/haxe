package cs.system.reflection.emit;

/** Defines the access modes for a dynamic assembly. */
@:native("System.Reflection.Emit.AssemblyBuilderAccess")
extern enum abstract AssemblyBuilderAccess(Int) {
	var Run = 1;
	var RunAndCollect = 9;
	@:op(A | B) static function or(lhs:AssemblyBuilderAccess, rhs:AssemblyBuilderAccess):AssemblyBuilderAccess;
	@:op(A & B) static function and(lhs:AssemblyBuilderAccess, rhs:AssemblyBuilderAccess):AssemblyBuilderAccess;
	@:op(A ^ B) static function xor(lhs:AssemblyBuilderAccess, rhs:AssemblyBuilderAccess):AssemblyBuilderAccess;
	@:op(~A) static function complement(value:AssemblyBuilderAccess):AssemblyBuilderAccess;
}
