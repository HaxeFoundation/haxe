package cs.system.runtime.compilerservices;

/** Specifies a source  in another assembly. */
@:native("System.Runtime.CompilerServices.TypeForwardedFromAttribute")
extern class TypeForwardedFromAttribute extends cs.system.Attribute {
	/**
	 * Gets the assembly-qualified name of the source type.
	 * @return The assembly-qualified name of the source type.
	 */
	var AssemblyFullName(default, never):String;
	function new(assemblyFullName:String):Void;
}
