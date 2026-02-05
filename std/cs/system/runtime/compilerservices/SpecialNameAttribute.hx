package cs.system.runtime.compilerservices;

/** Indicates that a type or member is treated in a special way by the runtime or tools.  This class cannot be inherited. */
@:native("System.Runtime.CompilerServices.SpecialNameAttribute")
extern class SpecialNameAttribute extends cs.system.Attribute {
	function new():Void;
}
