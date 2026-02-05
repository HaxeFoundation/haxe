package cs.system.runtime.compilerservices;

/** Marks a program element as read-only. */
@:native("System.Runtime.CompilerServices.IsReadOnlyAttribute")
extern class IsReadOnlyAttribute extends cs.system.Attribute {
	function new():Void;
}
