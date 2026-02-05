package cs.system.runtime.serialization;

/** When applied to the member of a type, specifies that the member is part of a data contract and is serializable by the . */
@:native("System.Runtime.Serialization.DataMemberAttribute")
extern class DataMemberAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets a value that specifies whether to serialize the default value for a
	 * field or property being serialized.
	 * @return if the default value for a member should be generated in the
	 * serialization stream; otherwise, . The default is .
	 */
	var EmitDefaultValue(default, default):Bool;
	/**
	 * Gets whether  has been explicitly set.
	 * @return if the name has been explicitly set; otherwise, .
	 */
	var IsNameSetExplicitly(default, never):Bool;
	/**
	 * Gets or sets a value that instructs the serialization engine that the member
	 * must be present when reading or deserializing.
	 * @return , if the member is required; otherwise, .
	 */
	var IsRequired(default, default):Bool;
	/**
	 * Gets or sets a data member name.
	 * @return The name of the data member. The default is the name of the target that
	 * the attribute is applied to.
	 */
	var Name(default, default):String;
	/**
	 * Gets or sets the order of serialization and deserialization of a member.
	 * @return The numeric order of serialization or deserialization.
	 */
	var Order(default, default):Int;
	function new():Void;
}
