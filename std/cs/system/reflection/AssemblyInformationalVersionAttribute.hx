package cs.system.reflection;

/** Defines additional version information for an assembly manifest. */
@:native("System.Reflection.AssemblyInformationalVersionAttribute")
extern class AssemblyInformationalVersionAttribute extends cs.system.Attribute {
	/**
	 * Gets version information.
	 * @return A string containing the version information.
	 */
	var InformationalVersion(default, never):String;
	function new(informationalVersion:String):Void;
}
