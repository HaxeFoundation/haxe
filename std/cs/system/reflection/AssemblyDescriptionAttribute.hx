package cs.system.reflection;

/** Provides a text description for an assembly. */
@:native("System.Reflection.AssemblyDescriptionAttribute")
extern class AssemblyDescriptionAttribute extends cs.system.Attribute {
	/**
	 * Gets assembly description information.
	 * @return A string containing the assembly description.
	 */
	var Description(default, never):String;
	function new(description:String):Void;
}
