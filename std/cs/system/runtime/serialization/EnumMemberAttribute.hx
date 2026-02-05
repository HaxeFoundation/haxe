package cs.system.runtime.serialization;

/** Specifies that the field is an enumeration member and should be serialized. */
@:native("System.Runtime.Serialization.EnumMemberAttribute")
extern class EnumMemberAttribute extends cs.system.Attribute {
	/**
	 * Gets whether the  has been explicitly set.
	 * @return if the value has been explicitly set; otherwise, .
	 */
	var IsValueSetExplicitly(default, never):Bool;
	/**
	 * Gets or sets the value associated with the enumeration member the attribute is
	 * applied to.
	 * @return The value associated with the enumeration member.
	 */
	var Value(default, default):String;
	function new():Void;
}
