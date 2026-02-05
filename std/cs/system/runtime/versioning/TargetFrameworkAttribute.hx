package cs.system.runtime.versioning;

/** Identifies the version of the .NET Framework that a particular assembly was compiled against. */
@:native("System.Runtime.Versioning.TargetFrameworkAttribute")
extern class TargetFrameworkAttribute extends cs.system.Attribute {
	/**
	 * Gets the display name of the .NET Framework version against which an assembly
	 * was built.
	 * @return The display name of the .NET Framework version.
	 */
	var FrameworkDisplayName(default, default):String;
	/**
	 * Gets the name of the .NET Framework version against which a particular assembly
	 * was compiled.
	 * @return The name of the .NET Framework version with which the assembly was
	 * compiled.
	 */
	var FrameworkName(default, never):String;
	function new(frameworkName:String):Void;
}
