package cs.system.reflection;

/** Specifies which culture the assembly supports. */
@:native("System.Reflection.AssemblyCultureAttribute")
extern class AssemblyCultureAttribute extends cs.system.Attribute {
	/**
	 * Gets the supported culture of the attributed assembly.
	 * @return A string containing the name of the supported culture.
	 */
	var Culture(default, never):String;
	function new(culture:String):Void;
}
