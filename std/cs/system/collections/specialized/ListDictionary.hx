package cs.system.collections.specialized;

/** Implements  using a singly linked list. Recommended for collections that typically include fewer than 10 items. */
@:native("System.Collections.Specialized.ListDictionary")
extern class ListDictionary {
	/**
	 * Gets the number of key/value pairs contained in the .
	 * @return The number of key/value pairs contained in the .
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value indicating whether the  has a fixed size.
	 * @return This property always returns .
	 */
	var IsFixedSize(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is read-only.
	 * @return This property always returns .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is synchronized (thread safe).
	 * @return This property always returns .
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets an  containing the keys in the .
	 * @return An  containing the keys in the .
	 */
	var Keys(default, never):cs.system.collections.ICollection;
	/**
	 * Gets an object that can be used to synchronize access to the .
	 * @return An object that can be used to synchronize access to the .
	 */
	var SyncRoot(default, never):Dynamic;
	/**
	 * Gets an  containing the values in the .
	 * @return An  containing the values in the .
	 */
	var Values(default, never):cs.system.collections.ICollection;
	@:native("get_Item")
	function get_Item(index0:Dynamic):Dynamic;
	@:native("set_Item")
	function set_Item(index0:Dynamic, value:Dynamic):Void;
	@:overload(function():Void {})
	function new(comparer:cs.system.collections.IComparer):Void;
	/**
	 * Adds an entry with the specified key and value into the .
	 * @param key The key of the entry to add.
	 * @param value The value of the entry to add. The value can be .
	 */
	function Add(key:Dynamic, value:Dynamic):Void;
	/** Removes all entries from the . */
	function Clear():Void;
	/**
	 * Determines whether the  contains a specific key.
	 * @param key The key to locate in the .
	 * @return if the  contains an entry with the specified key; otherwise, .
	 */
	function Contains(key:Dynamic):Bool;
	/**
	 * Copies the  entries to a one-dimensional  instance at the specified index.
	 * @param array The one-dimensional  that is the destination of the  objects copied
	 * from . The  must have zero-based indexing.
	 * @param index The zero-based index in  at which copying begins.
	 */
	function CopyTo(array:cs.system.Array, index:Int):Void;
	/**
	 * Returns an  that iterates through the .
	 * @return An  for the .
	 */
	function GetEnumerator():cs.system.collections.IDictionaryEnumerator;
	/**
	 * Removes the entry with the specified key from the .
	 * @param key The key of the entry to remove.
	 */
	function Remove(key:Dynamic):Void;
}
