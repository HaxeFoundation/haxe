package cs.system.runtime.compilerservices;

/** Identifies an assembly as a reference assembly, which contains metadata but no executable code. */
@:native("System.Runtime.CompilerServices.ReferenceAssemblyAttribute")
extern class ReferenceAssemblyAttribute extends cs.system.Attribute {
	/**
	 * Gets the description of the reference assembly.
	 * @return The description of the reference assembly.
	 */
	var Description(default, never):String;
	@:overload(function():Void {})
	function new(description:String):Void;
}
