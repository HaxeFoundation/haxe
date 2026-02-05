package cs.system.runtime.compilerservices;

/** Defines the details of how a method is implemented. */
@:native("System.Runtime.CompilerServices.MethodImplOptions")
extern enum abstract MethodImplOptions(Int) {
	var AggressiveInlining = 256;
	var ForwardRef = 16;
	var InternalCall = 4096;
	var NoInlining = 8;
	var NoOptimization = 64;
	var PreserveSig = 128;
	var Synchronized = 32;
	var Unmanaged = 4;
	@:op(A | B) static function or(lhs:MethodImplOptions, rhs:MethodImplOptions):MethodImplOptions;
	@:op(A & B) static function and(lhs:MethodImplOptions, rhs:MethodImplOptions):MethodImplOptions;
	@:op(A ^ B) static function xor(lhs:MethodImplOptions, rhs:MethodImplOptions):MethodImplOptions;
	@:op(~A) static function complement(value:MethodImplOptions):MethodImplOptions;
}
