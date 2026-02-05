package cs.system.collections.specialized;

/** Implements a hash table with the key and the value strongly typed to be strings rather than objects. */
@:native("System.Collections.Specialized.StringDictionary")
extern class StringDictionary {
	/**
	 * Gets the number of key/value pairs in the .
	 * @return The number of key/value pairs in the . Retrieving the value of this
	 * property is an O(1) operation.
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value indicating whether access to the  is synchronized (thread safe).
	 * @return if access to the  is synchronized (thread safe); otherwise, .
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets a collection of keys in the .
	 * @return An  that provides the keys in the .
	 */
	var Keys(default, never):cs.system.collections.ICollection;
	/**
	 * Gets an object that can be used to synchronize access to the .
	 * @return An  that can be used to synchronize access to the .
	 */
	var SyncRoot(default, never):Dynamic;
	/**
	 * Gets a collection of values in the .
	 * @return An  that provides the values in the .
	 */
	var Values(default, never):cs.system.collections.ICollection;
	@:native("get_Item")
	function get_Item(index0:String):String;
	@:native("set_Item")
	function set_Item(index0:String, value:String):Void;
	function new():Void;
	/**
	 * Adds an entry with the specified key and value into the .
	 * @param key The key of the entry to add.
	 * @param value The value of the entry to add. The value can be .
	 */
	function Add(key:String, value:String):Void;
	/** Removes all entries from the . */
	function Clear():Void;
	/**
	 * Determines if the  contains a specific key.
	 * @param key The key to locate in the .
	 * @return if the  contains an entry with the specified key; otherwise, .
	 */
	function ContainsKey(key:String):Bool;
	/**
	 * Determines if the  contains a specific value.
	 * @param value The value to locate in the . The value can be .
	 * @return if the  contains an element with the specified value; otherwise, .
	 */
	function ContainsValue(value:String):Bool;
	/**
	 * Copies the string dictionary values to a one-dimensional  instance at the
	 * specified index.
	 * @param array The one-dimensional  that is the destination of the values copied
	 * from the .
	 * @param index The index in the array where copying begins.
	 */
	function CopyTo(array:cs.system.Array, index:Int):Void;
	/**
	 * Returns an enumerator that iterates through the string dictionary.
	 * @return An  that iterates through the string dictionary.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * Removes the entry with the specified key from the string dictionary.
	 * @param key The key of the entry to remove.
	 */
	function Remove(key:String):Void;
}
