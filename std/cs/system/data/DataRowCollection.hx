package cs.system.data;

/** Represents a collection of rows for a . */
@:native("System.Data.DataRowCollection")
extern class DataRowCollection extends cs.system.data.InternalDataCollectionBase {
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.data.DataRow;
	@:overload(function(row:cs.system.data.DataRow):Void {})
	/**
	 * Adds the specified  to the  object.
	 * @param row The  to add.
	 */
	function Add(values:cs.NativeArray<Dynamic>):cs.system.data.DataRow;
	/** Clears the collection of all rows. */
	function Clear():Void;
	@:overload(function(key:Dynamic):Bool {})
	/**
	 * Gets a value that indicates whether the primary key of any row in the collection
	 * contains the specified value.
	 * @param key The value of the primary key to test for.
	 * @return if the collection contains a  with the specified primary key value;
	 * otherwise, .
	 */
	function Contains(keys:cs.NativeArray<Dynamic>):Bool;
	@:overload(function(ar:cs.system.Array, index:Int):Void {})
	/**
	 * Copies all the  objects from the collection into the given array, starting at
	 * the given destination array index.
	 * @param ar The one-dimensional array that is the destination of the elements
	 * copied from the . The array must have zero-based indexing.
	 * @param index The zero-based index in the array at which copying begins.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.data.DataRow>, index:Int):Void;
	@:overload(function(key:Dynamic):cs.system.data.DataRow {})
	/**
	 * Gets the row specified by the primary key value.
	 * @param key The primary key value of the  to find.
	 * @return A  that contains the primary key value specified; otherwise a null value
	 * if the primary key value does not exist in the .
	 */
	function Find(keys:cs.NativeArray<Dynamic>):cs.system.data.DataRow;
	/**
	 * Gets an  for this collection.
	 * @return An  for this collection.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * Gets the index of the specified  object.
	 * @param row The  to search for.
	 * @return The zero-based index of the row, or -1 if the row is not found in the
	 * collection.
	 */
	function IndexOf(row:cs.system.data.DataRow):Int;
	/**
	 * Inserts a new row into the collection at the specified location.
	 * @param row The  to add.
	 * @param pos The (zero-based) location in the collection where you want to add the
	 * .
	 */
	function InsertAt(row:cs.system.data.DataRow, pos:Int):Void;
	/**
	 * Removes the specified  from the collection.
	 * @param row The  to remove.
	 */
	function Remove(row:cs.system.data.DataRow):Void;
	/**
	 * Removes the row at the specified index from the collection.
	 * @param index The index of the row to remove.
	 */
	function RemoveAt(index:Int):Void;
}
