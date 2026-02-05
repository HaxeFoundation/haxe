package cs.system.reflection;

/** Specifies the version of the assembly being attributed. */
@:native("System.Reflection.AssemblyVersionAttribute")
extern class AssemblyVersionAttribute extends cs.system.Attribute {
	/**
	 * Gets the version number of the attributed assembly.
	 * @return A string containing the assembly version number.
	 */
	var Version(default, never):String;
	function new(version:String):Void;
}
