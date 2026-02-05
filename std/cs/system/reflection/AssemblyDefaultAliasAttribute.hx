package cs.system.reflection;

/** Defines a friendly default alias for an assembly manifest. */
@:native("System.Reflection.AssemblyDefaultAliasAttribute")
extern class AssemblyDefaultAliasAttribute extends cs.system.Attribute {
	/**
	 * Gets default alias information.
	 * @return A string containing the default alias information.
	 */
	var DefaultAlias(default, never):String;
	function new(defaultAlias:String):Void;
}
