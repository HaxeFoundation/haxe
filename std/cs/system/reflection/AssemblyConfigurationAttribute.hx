package cs.system.reflection;

/** Specifies the build configuration, such as retail or debug, for an assembly. */
@:native("System.Reflection.AssemblyConfigurationAttribute")
extern class AssemblyConfigurationAttribute extends cs.system.Attribute {
	/**
	 * Gets assembly configuration information.
	 * @return A string containing the assembly configuration information.
	 */
	var Configuration(default, never):String;
	function new(configuration:String):Void;
}
