package cs.system.collections;

/** Represents a nongeneric collection of key/value pairs. */
@:native("System.Collections.IDictionary")
extern interface IDictionary extends cs.system.collections.ICollection extends cs.system.collections.IEnumerable {
	/**
	 * Gets a value indicating whether the  object has a fixed size.
	 * @return if the  object has a fixed size; otherwise, .
	 */
	var IsFixedSize(default, never):Bool;
	/**
	 * Gets a value indicating whether the  object is read-only.
	 * @return if the  object is read-only; otherwise, .
	 */
	var IsReadOnly(default, never):Bool;
	@:native("get_Item")
	function get_Item(index0:Dynamic):Dynamic;
	@:native("set_Item")
	function set_Item(index0:Dynamic, value:Dynamic):Void;
	/**
	 * Gets an  object containing the keys of the  object.
	 * @return An  object containing the keys of the  object.
	 */
	var Keys(default, never):cs.system.collections.ICollection;
	/**
	 * Gets an  object containing the values in the  object.
	 * @return An  object containing the values in the  object.
	 */
	var Values(default, never):cs.system.collections.ICollection;
	/**
	 * Adds an element with the provided key and value to the  object.
	 * @param key The  to use as the key of the element to add.
	 * @param value The  to use as the value of the element to add.
	 */
	function Add(key:Dynamic, value:Dynamic):Void;
	/** Removes all elements from the  object. */
	function Clear():Void;
	/**
	 * Determines whether the  object contains an element with the specified key.
	 * @param key The key to locate in the  object.
	 * @return if the  contains an element with the key; otherwise, .
	 */
	function Contains(key:Dynamic):Bool;
	/**
	 * Returns an  object for the  object.
	 * @return An  object for the  object.
	 */
	function GetEnumerator():cs.system.collections.IDictionaryEnumerator;
	/**
	 * Removes the element with the specified key from the  object.
	 * @param key The key of the element to remove.
	 */
	function Remove(key:Dynamic):Void;
}
