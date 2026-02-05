package cs.system.runtime.interopservices;

/** Indicates the COM alias for a parameter or field type. */
@:native("System.Runtime.InteropServices.ComAliasNameAttribute")
extern class ComAliasNameAttribute extends cs.system.Attribute {
	/**
	 * Gets the alias for the field or parameter as found in the type library when it
	 * was imported.
	 * @return The alias for the field or parameter as found in the type library when
	 * it was imported.
	 */
	var Value(default, never):String;
	function new(alias:String):Void;
}
