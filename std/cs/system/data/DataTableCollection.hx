package cs.system.data;

/** Represents the collection of tables for the . */
@:native("System.Data.DataTableCollection")
extern class DataTableCollection extends cs.system.data.InternalDataCollectionBase {
	@:overload(function(index0:Int):cs.system.data.DataTable {})
	@:overload(function(index0:String):cs.system.data.DataTable {})
	@:native("get_Item")
	function get_Item(index0:String, index1:String):cs.system.data.DataTable;
	@:overload(function():cs.system.data.DataTable {})
	@:overload(function(table:cs.system.data.DataTable):Void {})
	@:overload(function(name:String):cs.system.data.DataTable {})
	/**
	 * Creates a new  object by using a default name and adds it to the collection.
	 * @return The newly created .
	 */
	function Add(name:String, tableNamespace:String):cs.system.data.DataTable;
	/**
	 * Copies the elements of the specified  array to the end of the collection.
	 * @param tables The array of  objects to add to the collection.
	 */
	function AddRange(tables:cs.NativeArray<cs.system.data.DataTable>):Void;
	/**
	 * Verifies whether the specified  object can be removed from the collection.
	 * @param table The  in the collection to perform the check against.
	 * @return if the table can be removed; otherwise .
	 */
	function CanRemove(table:cs.system.data.DataTable):Bool;
	/** Clears the collection of all  objects. */
	function Clear():Void;
	@:overload(function(name:String):Bool {})
	/**
	 * Gets a value that indicates whether a  object with the specified name exists in
	 * the collection.
	 * @param name The name of the  to find.
	 * @return if the specified table exists; otherwise .
	 */
	function Contains(name:String, tableNamespace:String):Bool;
	/**
	 * Copies all the elements of the current  to a one-dimensional , starting at the
	 * specified destination array index.
	 * @param array The one-dimensional  to copy the current  object's elements into.
	 * @param index The destination  index to start copying into.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.data.DataTable>, index:Int):Void;
	@:overload(function(table:cs.system.data.DataTable):Int {})
	@:overload(function(tableName:String):Int {})
	/**
	 * Gets the index of the specified  object.
	 * @param table The  to search for.
	 * @return The zero-based index of the table, or -1 if the table is not found in
	 * the collection.
	 */
	function IndexOf(tableName:String, tableNamespace:String):Int;
	@:overload(function(table:cs.system.data.DataTable):Void {})
	@:overload(function(name:String):Void {})
	/**
	 * Removes the specified  object from the collection.
	 * @param table The  to remove.
	 */
	function Remove(name:String, tableNamespace:String):Void;
	/**
	 * Removes the  object at the specified index from the collection.
	 * @param index The index of the  to remove.
	 */
	function RemoveAt(index:Int):Void;
}
