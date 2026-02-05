package cs.system.runtime.compilerservices;

/** Specifies that a type contains an unmanaged array that might potentially overflow. This class cannot be inherited. */
@:native("System.Runtime.CompilerServices.UnsafeValueTypeAttribute")
extern class UnsafeValueTypeAttribute extends cs.system.Attribute {
	function new():Void;
}
