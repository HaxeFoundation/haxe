package cs.system.reflection;

/** Instructs a compiler to use a specific version number for the Win32 file version resource. The Win32 file version is not required to be the same as the assembly's version number. */
@:native("System.Reflection.AssemblyFileVersionAttribute")
extern class AssemblyFileVersionAttribute extends cs.system.Attribute {
	/**
	 * Gets the Win32 file version resource name.
	 * @return A string containing the file version resource name.
	 */
	var Version(default, never):String;
	function new(version:String):Void;
}
