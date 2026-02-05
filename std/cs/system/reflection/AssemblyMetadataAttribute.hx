package cs.system.reflection;

/** Defines a key/value metadata pair for the decorated assembly. */
@:native("System.Reflection.AssemblyMetadataAttribute")
extern class AssemblyMetadataAttribute extends cs.system.Attribute {
	/**
	 * Gets the metadata key.
	 * @return The metadata key.
	 */
	var Key(default, never):String;
	/**
	 * Gets the metadata value.
	 * @return The metadata value.
	 */
	var Value(default, never):String;
	function new(key:String, value:String):Void;
}
