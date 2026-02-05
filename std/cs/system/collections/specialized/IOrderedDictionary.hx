package cs.system.collections.specialized;

/** Represents an indexed collection of key/value pairs. */
@:native("System.Collections.Specialized.IOrderedDictionary")
extern interface IOrderedDictionary extends cs.system.collections.ICollection extends cs.system.collections.IEnumerable extends cs.system.collections.IDictionary {
	@:native("get_Item")
	function get_Item(index0:Int):Dynamic;
	@:native("set_Item")
	function set_Item(index0:Int, value:Dynamic):Void;
	/**
	 * Returns an enumerator that iterates through the  collection.
	 * @return An  for the entire  collection.
	 */
	function GetEnumerator():cs.system.collections.IDictionaryEnumerator;
	/**
	 * Inserts a key/value pair into the collection at the specified index.
	 * @param index The zero-based index at which the key/value pair should be
	 * inserted.
	 * @param key The object to use as the key of the element to add.
	 * @param value The object to use as the value of the element to add.  The value
	 * can be .
	 */
	function Insert(index:Int, key:Dynamic, value:Dynamic):Void;
	/**
	 * Removes the element at the specified index.
	 * @param index The zero-based index of the element to remove.
	 */
	function RemoveAt(index:Int):Void;
}
