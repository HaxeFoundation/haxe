package cs.system.collections;

/** Represents a non-generic collection of objects that can be individually accessed by index. */
@:native("System.Collections.IList")
extern interface IList extends cs.system.collections.ICollection extends cs.system.collections.IEnumerable {
	/**
	 * Gets a value indicating whether the  has a fixed size.
	 * @return if the  has a fixed size; otherwise, .
	 */
	var IsFixedSize(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is read-only.
	 * @return if the  is read-only; otherwise, .
	 */
	var IsReadOnly(default, never):Bool;
	@:native("get_Item")
	function get_Item(index0:Int):Dynamic;
	@:native("set_Item")
	function set_Item(index0:Int, value:Dynamic):Void;
	/**
	 * Adds an item to the .
	 * @param value The object to add to the .
	 * @return The position into which the new element was inserted, or -1 to indicate
	 * that the item was not inserted into the collection.
	 */
	function Add(value:Dynamic):Int;
	/** Removes all items from the . */
	function Clear():Void;
	/**
	 * Determines whether the  contains a specific value.
	 * @param value The object to locate in the .
	 * @return if the  is found in the ; otherwise, .
	 */
	function Contains(value:Dynamic):Bool;
	/**
	 * Determines the index of a specific item in the .
	 * @param value The object to locate in the .
	 * @return The index of  if found in the list; otherwise, -1.
	 */
	function IndexOf(value:Dynamic):Int;
	/**
	 * Inserts an item to the  at the specified index.
	 * @param index The zero-based index at which  should be inserted.
	 * @param value The object to insert into the .
	 */
	function Insert(index:Int, value:Dynamic):Void;
	/**
	 * Removes the first occurrence of a specific object from the .
	 * @param value The object to remove from the .
	 */
	function Remove(value:Dynamic):Void;
	/**
	 * Removes the  item at the specified index.
	 * @param index The zero-based index of the item to remove.
	 */
	function RemoveAt(index:Int):Void;
}
