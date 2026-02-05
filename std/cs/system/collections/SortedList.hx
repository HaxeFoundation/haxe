package cs.system.collections;

/** Represents a collection of key/value pairs that are sorted by the keys and are accessible by key and by index. */
@:native("System.Collections.SortedList")
extern class SortedList {
	/**
	 * Gets or sets the capacity of a  object.
	 * @return The number of elements that the  object can contain.
	 */
	var Capacity(default, default):Int;
	/**
	 * Gets the number of elements contained in a  object.
	 * @return The number of elements contained in the  object.
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value indicating whether a  object has a fixed size.
	 * @return if the  object has a fixed size; otherwise, . The default is .
	 */
	var IsFixedSize(default, never):Bool;
	/**
	 * Gets a value indicating whether a  object is read-only.
	 * @return if the  object is read-only; otherwise, . The default is .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets a value indicating whether access to a  object is synchronized (thread
	 * safe).
	 * @return if access to the  object is synchronized (thread safe); otherwise, . The
	 * default is .
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets the keys in a  object.
	 * @return An  object containing the keys in the  object.
	 */
	var Keys(default, never):cs.system.collections.ICollection;
	/**
	 * Gets an object that can be used to synchronize access to a  object.
	 * @return An object that can be used to synchronize access to the  object.
	 */
	var SyncRoot(default, never):Dynamic;
	/**
	 * Gets the values in a  object.
	 * @return An  object containing the values in the  object.
	 */
	var Values(default, never):cs.system.collections.ICollection;
	@:native("get_Item")
	function get_Item(index0:Dynamic):Dynamic;
	@:native("set_Item")
	function set_Item(index0:Dynamic, value:Dynamic):Void;
	@:overload(function():Void {})
	@:overload(function(comparer:cs.system.collections.IComparer):Void {})
	@:overload(function(d:cs.system.collections.IDictionary):Void {})
	@:overload(function(initialCapacity:Int):Void {})
	@:overload(function(comparer:cs.system.collections.IComparer, capacity:Int):Void {})
	function new(d:cs.system.collections.IDictionary, comparer:cs.system.collections.IComparer):Void;
	/**
	 * Returns a synchronized (thread-safe) wrapper for a  object.
	 * @param list The  object to synchronize.
	 * @return A synchronized (thread-safe) wrapper for the  object.
	 */
	static function Synchronized(list:cs.system.collections.SortedList):cs.system.collections.SortedList;
	/**
	 * Adds an element with the specified key and value to a  object.
	 * @param key The key of the element to add.
	 * @param value The value of the element to add. The value can be .
	 */
	function Add(key:Dynamic, value:Dynamic):Void;
	/** Removes all elements from a  object. */
	function Clear():Void;
	/**
	 * Creates a shallow copy of a  object.
	 * @return A shallow copy of the  object.
	 */
	function Clone():Dynamic;
	/**
	 * Determines whether a  object contains a specific key.
	 * @param key The key to locate in the  object.
	 * @return if the  object contains an element with the specified ; otherwise, .
	 */
	function Contains(key:Dynamic):Bool;
	/**
	 * Determines whether a  object contains a specific key.
	 * @param key The key to locate in the  object.
	 * @return if the  object contains an element with the specified ; otherwise, .
	 */
	function ContainsKey(key:Dynamic):Bool;
	/**
	 * Determines whether a  object contains a specific value.
	 * @param value The value to locate in the  object. The value can be .
	 * @return if the  object contains an element with the specified ; otherwise, .
	 */
	function ContainsValue(value:Dynamic):Bool;
	/**
	 * Copies  elements to a one-dimensional  object, starting at the specified index
	 * in the array.
	 * @param array The one-dimensional  object that is the destination of the  objects
	 * copied from . The  must have zero-based indexing.
	 * @param arrayIndex The zero-based index in  at which copying begins.
	 */
	function CopyTo(array:cs.system.Array, arrayIndex:Int):Void;
	/**
	 * Gets the value at the specified index of a  object.
	 * @param index The zero-based index of the value to get.
	 * @return The value at the specified index of the  object.
	 */
	function GetByIndex(index:Int):Dynamic;
	/**
	 * Returns an  object that iterates through a  object.
	 * @return An  object for the  object.
	 */
	function GetEnumerator():cs.system.collections.IDictionaryEnumerator;
	/**
	 * Gets the key at the specified index of a  object.
	 * @param index The zero-based index of the key to get.
	 * @return The key at the specified index of the  object.
	 */
	function GetKey(index:Int):Dynamic;
	/**
	 * Gets the keys in a  object.
	 * @return An  object containing the keys in the  object.
	 */
	function GetKeyList():cs.system.collections.IList;
	/**
	 * Gets the values in a  object.
	 * @return An  object containing the values in the  object.
	 */
	function GetValueList():cs.system.collections.IList;
	/**
	 * Returns the zero-based index of the specified key in a  object.
	 * @param key The key to locate in the  object.
	 * @return The zero-based index of the  parameter, if  is found in the  object;
	 * otherwise, -1.
	 */
	function IndexOfKey(key:Dynamic):Int;
	/**
	 * Returns the zero-based index of the first occurrence of the specified value in a
	 * object.
	 * @param value The value to locate in the  object. The value can be .
	 * @return The zero-based index of the first occurrence of the  parameter, if  is
	 * found in the  object; otherwise, -1.
	 */
	function IndexOfValue(value:Dynamic):Int;
	/**
	 * Removes the element with the specified key from a  object.
	 * @param key The key of the element to remove.
	 */
	function Remove(key:Dynamic):Void;
	/**
	 * Removes the element at the specified index of a  object.
	 * @param index The zero-based index of the element to remove.
	 */
	function RemoveAt(index:Int):Void;
	/**
	 * Replaces the value at a specific index in a  object.
	 * @param index The zero-based index at which to save .
	 * @param value The  to save into the  object. The value can be .
	 */
	function SetByIndex(index:Int, value:Dynamic):Void;
	/** Sets the capacity to the actual number of elements in a  object. */
	function TrimToSize():Void;
}
