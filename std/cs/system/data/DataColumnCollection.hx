package cs.system.data;

/** Represents a collection of  objects for a . */
@:native("System.Data.DataColumnCollection")
extern class DataColumnCollection extends cs.system.data.InternalDataCollectionBase {
	@:overload(function(index0:Int):cs.system.data.DataColumn {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.data.DataColumn;
	@:overload(function():cs.system.data.DataColumn {})
	@:overload(function(column:cs.system.data.DataColumn):Void {})
	@:overload(function(columnName:String):cs.system.data.DataColumn {})
	@:overload(function(columnName:String, type:cs.system.Type):cs.system.data.DataColumn {})
	/**
	 * Creates and adds a  object to the .
	 * @return The newly created .
	 */
	function Add(columnName:String, type:cs.system.Type, expression:String):cs.system.data.DataColumn;
	/**
	 * Copies the elements of the specified  array to the end of the collection.
	 * @param columns The array of  objects to add to the collection.
	 */
	function AddRange(columns:cs.NativeArray<cs.system.data.DataColumn>):Void;
	/**
	 * Checks whether a specific column can be removed from the collection.
	 * @param column A  in the collection.
	 * @return if the column can be removed.  if, The  parameter is . The column does
	 * not belong to this collection. The column is part of a relationship. Another
	 * column's expression depends on this column.
	 */
	function CanRemove(column:cs.system.data.DataColumn):Bool;
	/** Clears the collection of any columns. */
	function Clear():Void;
	/**
	 * Checks whether the collection contains a column with the specified name.
	 * @param name The  of the column to look for.
	 * @return if a column exists with this name; otherwise, .
	 */
	function Contains(name:String):Bool;
	/**
	 * Copies the entire collection into an existing array, starting at a specified
	 * index within the array.
	 * @param array An array of  objects to copy the collection into.
	 * @param index The index to start from.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.data.DataColumn>, index:Int):Void;
	@:overload(function(column:cs.system.data.DataColumn):Int {})
	/**
	 * Gets the index of a column specified by name.
	 * @param column The name of the column to return.
	 * @return The index of the column specified by  if it is found; otherwise, -1.
	 */
	function IndexOf(columnName:String):Int;
	@:overload(function(column:cs.system.data.DataColumn):Void {})
	/**
	 * Removes the specified  object from the collection.
	 * @param column The  to remove.
	 */
	function Remove(name:String):Void;
	/**
	 * Removes the column at the specified index from the collection.
	 * @param index The index of the column to remove.
	 */
	function RemoveAt(index:Int):Void;
}
