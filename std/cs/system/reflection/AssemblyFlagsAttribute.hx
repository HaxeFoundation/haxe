package cs.system.reflection;

/** Specifies a bitwise combination of  flags for an assembly, describing just-in-time (JIT) compiler options, whether the assembly is retargetable, and whether it has a full or tokenized public key. This class cannot be inherited. */
@:native("System.Reflection.AssemblyFlagsAttribute")
extern class AssemblyFlagsAttribute extends cs.system.Attribute {
	/**
	 * Gets an integer value representing the combination of  flags specified when this
	 * attribute instance was created.
	 * @return An integer value representing a bitwise combination of  flags.
	 */
	var AssemblyFlags(default, never):Int;
	/**
	 * Gets an unsigned integer value representing the combination of  flags specified
	 * when this attribute instance was created.
	 * @return An unsigned integer value representing a bitwise combination of  flags.
	 */
	var Flags(default, never):cs.UInt;
	@:overload(function(assemblyFlags:Int):Void {})
	@:overload(function(assemblyFlags:cs.system.reflection.AssemblyNameFlags):Void {})
	function new(flags:cs.UInt):Void;
}
