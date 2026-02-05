package cs.system.runtime.serialization;

/** Specifies that a field can be missing from a serialization stream so that the  and the  does not throw an exception. */
@:native("System.Runtime.Serialization.OptionalFieldAttribute")
extern class OptionalFieldAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets a version number to indicate when the optional field was added.
	 * @return The version of the .
	 */
	var VersionAdded(default, default):Int;
	function new():Void;
}
