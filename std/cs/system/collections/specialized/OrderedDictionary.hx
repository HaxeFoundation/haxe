package cs.system.collections.specialized;

/** Represents a collection of key/value pairs that are accessible by the key or index. */
@:native("System.Collections.Specialized.OrderedDictionary")
extern class OrderedDictionary {
	/**
	 * Gets the number of key/values pairs contained in the  collection.
	 * @return The number of key/value pairs contained in the  collection.
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value indicating whether the  collection is read-only.
	 * @return if the  collection is read-only; otherwise, . The default is .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets an  object containing the keys in the  collection.
	 * @return An  object containing the keys in the  collection.
	 */
	var Keys(default, never):cs.system.collections.ICollection;
	/**
	 * Gets an  object containing the values in the  collection.
	 * @return An  object containing the values in the  collection.
	 */
	var Values(default, never):cs.system.collections.ICollection;
	@:overload(function(index0:Int):Dynamic {})
	@:native("get_Item")
	function get_Item(index0:Dynamic):Dynamic;
	@:overload(function(index0:Int, value:Dynamic):Void {})
	@:native("set_Item")
	function set_Item(index0:Dynamic, value:Dynamic):Void;
	@:overload(function():Void {})
	@:overload(function(comparer:cs.system.collections.IEqualityComparer):Void {})
	@:overload(function(capacity:Int):Void {})
	function new(capacity:Int, comparer:cs.system.collections.IEqualityComparer):Void;
	/**
	 * Adds an entry with the specified key and value into the  collection with the
	 * lowest available index.
	 * @param key The key of the entry to add.
	 * @param value The value of the entry to add. This value can be .
	 */
	function Add(key:Dynamic, value:Dynamic):Void;
	/**
	 * Returns a read-only copy of the current  collection.
	 * @return A read-only copy of the current  collection.
	 */
	function AsReadOnly():cs.system.collections.specialized.OrderedDictionary;
	/** Removes all elements from the  collection. */
	function Clear():Void;
	/**
	 * Determines whether the  collection contains a specific key.
	 * @param key The key to locate in the  collection.
	 * @return if the  collection contains an element with the specified key;
	 * otherwise, .
	 */
	function Contains(key:Dynamic):Bool;
	/**
	 * Copies the  elements to a one-dimensional  object at the specified index.
	 * @param array The one-dimensional  object that is the destination of the  objects
	 * copied from  collection. The  must have zero-based indexing.
	 * @param index The zero-based index in  at which copying begins.
	 */
	function CopyTo(array:cs.system.Array, index:Int):Void;
	/**
	 * Returns an  object that iterates through the  collection.
	 * @return An  object for the  collection.
	 */
	function GetEnumerator():cs.system.collections.IDictionaryEnumerator;
	/**
	 * Implements the  interface and returns the data needed to serialize the 
	 * collection.
	 * @param info A  object containing the information required to serialize the 
	 * collection.
	 * @param context A  object containing the source and destination of the serialized
	 * stream associated with the .
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Inserts a new entry into the  collection with the specified key and value at the
	 * specified index.
	 * @param index The zero-based index at which the element should be inserted.
	 * @param key The key of the entry to add.
	 * @param value The value of the entry to add. The value can be .
	 */
	function Insert(index:Int, key:Dynamic, value:Dynamic):Void;
	/**
	 * Removes the entry with the specified key from the  collection.
	 * @param key The key of the entry to remove.
	 */
	function Remove(key:Dynamic):Void;
	/**
	 * Removes the entry at the specified index from the  collection.
	 * @param index The zero-based index of the entry to remove.
	 */
	function RemoveAt(index:Int):Void;
}
