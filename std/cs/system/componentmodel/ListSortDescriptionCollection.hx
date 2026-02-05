package cs.system.componentmodel;

/** Represents a collection of  objects. */
@:native("System.ComponentModel.ListSortDescriptionCollection")
extern class ListSortDescriptionCollection {
	/**
	 * Gets the number of items in the collection.
	 * @return The number of items in the collection.
	 */
	var Count(default, never):Int;
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.componentmodel.ListSortDescription;
	@:native("set_Item")
	function set_Item(index0:Int, value:cs.system.componentmodel.ListSortDescription):Void;
	@:overload(function():Void {})
	function new(sorts:cs.NativeArray<cs.system.componentmodel.ListSortDescription>):Void;
	/**
	 * Determines if the  contains a specific value.
	 * @param value The  to locate in the collection.
	 * @return if the  is found in the collection; otherwise, .
	 */
	function Contains(value:Dynamic):Bool;
	/**
	 * Copies the contents of the collection to the specified array, starting at the
	 * specified destination array index.
	 * @param array The destination array for the items copied from the collection.
	 * @param index The index of the destination array at which copying begins.
	 */
	function CopyTo(array:cs.system.Array, index:Int):Void;
	/**
	 * Returns the index of the specified item in the collection.
	 * @param value The  to locate in the collection.
	 * @return The index of  if found in the list; otherwise, -1.
	 */
	function IndexOf(value:Dynamic):Int;
}
