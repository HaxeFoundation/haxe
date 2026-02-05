package cs.system.runtime.compilerservices;

/** Marks a type definition as discardable. */
@:native("System.Runtime.CompilerServices.DiscardableAttribute")
extern class DiscardableAttribute extends cs.system.Attribute {
	function new():Void;
}
