package cs.system.runtime.compilerservices;

/** Applies metadata to an assembly that indicates that a type is an unmanaged type.  This class cannot be inherited. */
@:native("System.Runtime.CompilerServices.NativeCppClassAttribute")
extern class NativeCppClassAttribute extends cs.system.Attribute {
	function new():Void;
}
