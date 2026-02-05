package cs.system.reflection;

/** Provides information about an  reference. */
@:native("System.Reflection.AssemblyNameFlags")
extern enum abstract AssemblyNameFlags(Int) {
	var EnableJITcompileOptimizer = 16384;
	var EnableJITcompileTracking = 32768;
	var None = 0;
	var PublicKey = 1;
	var Retargetable = 256;
	@:op(A | B) static function or(lhs:AssemblyNameFlags, rhs:AssemblyNameFlags):AssemblyNameFlags;
	@:op(A & B) static function and(lhs:AssemblyNameFlags, rhs:AssemblyNameFlags):AssemblyNameFlags;
	@:op(A ^ B) static function xor(lhs:AssemblyNameFlags, rhs:AssemblyNameFlags):AssemblyNameFlags;
	@:op(~A) static function complement(value:AssemblyNameFlags):AssemblyNameFlags;
}
