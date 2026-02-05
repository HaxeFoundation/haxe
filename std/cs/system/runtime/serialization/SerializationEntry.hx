package cs.system.runtime.serialization;

/** Holds the value, , and name of a serialized object. */
@:native("System.Runtime.Serialization.SerializationEntry")
extern class SerializationEntry extends cs.system.ValueType {
	/**
	 * Gets the name of the object.
	 * @return The name of the object.
	 */
	var Name(default, never):String;
	/**
	 * Gets the  of the object.
	 * @return The  of the object.
	 */
	var ObjectType(default, never):cs.system.Type;
	/**
	 * Gets the value contained in the object.
	 * @return The value contained in the object.
	 */
	var Value(default, never):Dynamic;
}
