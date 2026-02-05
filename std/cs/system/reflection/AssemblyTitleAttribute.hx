package cs.system.reflection;

/** Specifies a description for an assembly. */
@:native("System.Reflection.AssemblyTitleAttribute")
extern class AssemblyTitleAttribute extends cs.system.Attribute {
	/**
	 * Gets assembly title information.
	 * @return The assembly title.
	 */
	var Title(default, never):String;
	function new(title:String):Void;
}
