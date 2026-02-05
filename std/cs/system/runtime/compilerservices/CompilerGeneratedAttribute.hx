package cs.system.runtime.compilerservices;

/** Distinguishes a compiler-generated element from a user-generated element. This class cannot be inherited. */
@:native("System.Runtime.CompilerServices.CompilerGeneratedAttribute")
extern class CompilerGeneratedAttribute extends cs.system.Attribute {
	function new():Void;
}
