package cs.system.runtime.serialization;

/** Provides a formatter-friendly mechanism for parsing the data in . This class cannot be inherited. */
@:native("System.Runtime.Serialization.SerializationInfoEnumerator")
extern class SerializationInfoEnumerator {
	/**
	 * Gets the item currently being examined.
	 * @return The item currently being examined.
	 */
	var Current(default, never):cs.system.runtime.serialization.SerializationEntry;
	/**
	 * Gets the name for the item currently being examined.
	 * @return The item name.
	 */
	var Name(default, never):String;
	/**
	 * Gets the type of the item currently being examined.
	 * @return The type of the item currently being examined.
	 */
	var ObjectType(default, never):cs.system.Type;
	/**
	 * Gets the value of the item currently being examined.
	 * @return The value of the item currently being examined.
	 */
	var Value(default, never):Dynamic;
	/**
	 * Updates the enumerator to the next item.
	 * @return if a new element is found; otherwise, .
	 */
	function MoveNext():Bool;
	/** Resets the enumerator to the first item. */
	function Reset():Void;
}
