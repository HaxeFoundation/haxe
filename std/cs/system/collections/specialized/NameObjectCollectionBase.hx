package cs.system.collections.specialized;

/** Provides the  base class for a collection of associated  keys and  values that can be accessed either with the key or with the index. */
@:native("System.Collections.Specialized.NameObjectCollectionBase")
extern class NameObjectCollectionBase {
	/**
	 * Gets the number of key/value pairs contained in the  instance.
	 * @return The number of key/value pairs contained in the  instance.
	 */
	var Count(default, never):Int;
	/**
	 * Gets or sets a value indicating whether the  instance is read-only.
	 * @return if the  instance is read-only; otherwise, .
	 */
	var IsReadOnly(default, default):Bool;
	/**
	 * Gets a  instance that contains all the keys in the  instance.
	 * @return A  instance that contains all the keys in the  instance.
	 */
	var Keys(default, never):cs.system.collections.specialized.NameObjectCollectionBase_KeysCollection;
	/**
	 * Returns an enumerator that iterates through the .
	 * @return An  for the  instance.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * Implements the  interface and returns the data needed to serialize the 
	 * instance.
	 * @param info A  object that contains the information required to serialize the 
	 * instance.
	 * @param context A  object that contains the source and destination of the
	 * serialized stream associated with the  instance.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Implements the  interface and raises the deserialization event when the
	 * deserialization is complete.
	 * @param sender The source of the deserialization event.
	 */
	function OnDeserialization(sender:Dynamic):Void;
}
