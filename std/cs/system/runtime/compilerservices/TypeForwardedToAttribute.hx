package cs.system.runtime.compilerservices;

/** Specifies a destination  in another assembly. */
@:native("System.Runtime.CompilerServices.TypeForwardedToAttribute")
extern class TypeForwardedToAttribute extends cs.system.Attribute {
	/**
	 * Gets the destination  in another assembly.
	 * @return The destination  in another assembly.
	 */
	var Destination(default, never):cs.system.Type;
	function new(destination:cs.system.Type):Void;
}
