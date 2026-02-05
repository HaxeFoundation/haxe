package cs.system.data.common;

/** A collection of  objects. This class cannot be inherited. */
@:native("System.Data.Common.DataTableMappingCollection")
extern class DataTableMappingCollection extends cs.system.MarshalByRefObject {
	/**
	 * Gets the number of  objects in the collection.
	 * @return The number of  objects in the collection.
	 */
	var Count(default, never):Int;
	@:overload(function(index0:Int):cs.system.data.common.DataTableMapping {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.data.common.DataTableMapping;
	@:overload(function(index0:Int, value:cs.system.data.common.DataTableMapping):Void {})
	@:native("set_Item")
	function set_Item(index0:String, value:cs.system.data.common.DataTableMapping):Void;
	function new():Void;
	/**
	 * Gets a  object with the specified source table name and  table name, using the
	 * given .
	 * @param tableMappings The  collection to search.
	 * @param sourceTable The case-sensitive name of the mapped source table.
	 * @param dataSetTable The name, which is not case-sensitive, of the mapped  table.
	 * @param mappingAction One of the  values.
	 * @return A  object.
	 */
	static function GetTableMappingBySchemaAction(tableMappings:cs.system.data.common.DataTableMappingCollection, sourceTable:String, dataSetTable:String, mappingAction:cs.system.data.MissingMappingAction):cs.system.data.common.DataTableMapping;
	@:overload(function(value:Dynamic):Int {})
	/**
	 * Adds an  that is a table mapping to the collection.
	 * @param value A  object to add to the collection.
	 * @return The index of the  object added to the collection.
	 */
	function Add(sourceTable:String, dataSetTable:String):cs.system.data.common.DataTableMapping;
	@:overload(function(values:cs.system.Array):Void {})
	/**
	 * Copies the elements of the specified  to the end of the collection.
	 * @param values An  of values to add to the collection.
	 */
	function AddRange(values:cs.NativeArray<cs.system.data.common.DataTableMapping>):Void;
	/** Removes all  objects from the collection. */
	function Clear():Void;
	@:overload(function(value:Dynamic):Bool {})
	/**
	 * Gets a value indicating whether the given  object exists in the collection.
	 * @param value An  that is the .
	 * @return if this collection contains the specified ; otherwise .
	 */
	function Contains(value:String):Bool;
	@:overload(function(array:cs.system.Array, index:Int):Void {})
	/**
	 * Copies the elements of the  to the specified array.
	 * @param array An  to which to copy the  elements.
	 * @param index The starting index of the array.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.data.common.DataTableMapping>, index:Int):Void;
	/**
	 * Gets the  object with the specified  table name.
	 * @param dataSetTable The name, which is not case-sensitive, of the  table to
	 * find.
	 * @return The  object with the specified  table name.
	 */
	function GetByDataSetTable(dataSetTable:String):cs.system.data.common.DataTableMapping;
	/**
	 * Gets an enumerator that can iterate through the collection.
	 * @return An  that can be used to iterate through the collection.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	@:overload(function(value:Dynamic):Int {})
	/**
	 * Gets the location of the specified  object within the collection.
	 * @param value An  that is the  object to find.
	 * @return The zero-based location of the specified  object within the collection.
	 */
	function IndexOf(sourceTable:String):Int;
	/**
	 * Gets the location of the  object with the specified  table name.
	 * @param dataSetTable The name, which is not case-sensitive, of the  table to
	 * find.
	 * @return The zero-based location of the  object with the given  table name, or -1
	 * if the  object does not exist in the collection.
	 */
	function IndexOfDataSetTable(dataSetTable:String):Int;
	@:overload(function(index:Int, value:cs.system.data.common.DataTableMapping):Void {})
	/**
	 * Inserts a  object into the  at the specified index.
	 * @param index The zero-based index of the  object to insert.
	 * @param value The  object to insert.
	 */
	function Insert(index:Int, value:Dynamic):Void;
	@:overload(function(value:cs.system.data.common.DataTableMapping):Void {})
	/**
	 * Removes the specified  object from the collection.
	 * @param value The  object to remove.
	 */
	function Remove(value:Dynamic):Void;
	@:overload(function(index:Int):Void {})
	/**
	 * Removes the  object located at the specified index from the collection.
	 * @param index The zero-based index of the  object to remove.
	 */
	function RemoveAt(sourceTable:String):Void;
}
