package cs.system.runtime.compilerservices;

/** Indicates that a method is an extension method, or that a class or assembly contains extension methods. */
@:native("System.Runtime.CompilerServices.ExtensionAttribute")
extern class ExtensionAttribute extends cs.system.Attribute {
	function new():Void;
}
