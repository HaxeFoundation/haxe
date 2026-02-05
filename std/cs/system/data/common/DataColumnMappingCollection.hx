package cs.system.data.common;

/** Contains a collection of  objects. */
@:native("System.Data.Common.DataColumnMappingCollection")
extern class DataColumnMappingCollection extends cs.system.MarshalByRefObject {
	/**
	 * Gets the number of  objects in the collection.
	 * @return The number of items in the collection.
	 */
	var Count(default, never):Int;
	@:overload(function(index0:Int):cs.system.data.common.DataColumnMapping {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.data.common.DataColumnMapping;
	@:overload(function(index0:Int, value:cs.system.data.common.DataColumnMapping):Void {})
	@:native("set_Item")
	function set_Item(index0:String, value:cs.system.data.common.DataColumnMapping):Void;
	function new():Void;
	/**
	 * Gets a  for the specified , source column name, and .
	 * @param columnMappings The .
	 * @param sourceColumn The case-sensitive source column name to find.
	 * @param mappingAction One of the  values.
	 * @return A  object.
	 */
	static function GetColumnMappingBySchemaAction(columnMappings:cs.system.data.common.DataColumnMappingCollection, sourceColumn:String, mappingAction:cs.system.data.MissingMappingAction):cs.system.data.common.DataColumnMapping;
	/**
	 * A static method that returns a  object without instantiating a  object.
	 * @param columnMappings The .
	 * @param sourceColumn The case-sensitive column name from a data source.
	 * @param dataType The data type for the column being mapped.
	 * @param dataTable An instance of .
	 * @param mappingAction One of the  values.
	 * @param schemaAction Determines the action to take when the existing  schema does
	 * not match incoming data.
	 * @return A  object.
	 */
	static function GetDataColumn(columnMappings:cs.system.data.common.DataColumnMappingCollection, sourceColumn:String, dataType:cs.system.Type, dataTable:cs.system.data.DataTable, mappingAction:cs.system.data.MissingMappingAction, schemaAction:cs.system.data.MissingSchemaAction):cs.system.data.DataColumn;
	@:overload(function(value:Dynamic):Int {})
	/**
	 * Adds a  object to the collection.
	 * @param value A  object to add to the collection.
	 * @return The index of the  object that was added to the collection.
	 */
	function Add(sourceColumn:String, dataSetColumn:String):cs.system.data.common.DataColumnMapping;
	@:overload(function(values:cs.system.Array):Void {})
	/**
	 * Copies the elements of the specified  to the end of the collection.
	 * @param values The  to add to the collection.
	 */
	function AddRange(values:cs.NativeArray<cs.system.data.common.DataColumnMapping>):Void;
	/** Removes all  objects from the collection. */
	function Clear():Void;
	@:overload(function(value:Dynamic):Bool {})
	/**
	 * Gets a value indicating whether a  object with the given  exists in the
	 * collection.
	 * @param value An  that is the .
	 * @return if the collection contains the specified  object; otherwise, .
	 */
	function Contains(value:String):Bool;
	@:overload(function(array:cs.system.Array, index:Int):Void {})
	/**
	 * Copies the elements of the  to the specified array.
	 * @param array An  to which to copy  elements.
	 * @param index The starting index of the array.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.data.common.DataColumnMapping>, index:Int):Void;
	/**
	 * Gets the  object with the specified  column name.
	 * @param value The name, which is not case-sensitive, of the  column to find.
	 * @return The  object with the specified  column name.
	 */
	function GetByDataSetColumn(value:String):cs.system.data.common.DataColumnMapping;
	/**
	 * Gets an enumerator that can iterate through the collection.
	 * @return An  that can be used to iterate through the collection.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	@:overload(function(value:Dynamic):Int {})
	/**
	 * Gets the location of the specified  that is a  within the collection.
	 * @param value An  that is the  to find.
	 * @return The zero-based location of the specified  that is a  within the
	 * collection.
	 */
	function IndexOf(sourceColumn:String):Int;
	/**
	 * Gets the location of the specified  with the given  column name.
	 * @param dataSetColumn The name, which is not case-sensitive, of the data set
	 * column to find.
	 * @return The zero-based location of the specified  with the given  column name,
	 * or -1 if the  object does not exist in the collection.
	 */
	function IndexOfDataSetColumn(dataSetColumn:String):Int;
	@:overload(function(index:Int, value:cs.system.data.common.DataColumnMapping):Void {})
	/**
	 * Inserts a  object into the  at the specified index.
	 * @param index The zero-based index of the  object to insert.
	 * @param value The  object.
	 */
	function Insert(index:Int, value:Dynamic):Void;
	@:overload(function(value:cs.system.data.common.DataColumnMapping):Void {})
	/**
	 * Removes the specified  from the collection.
	 * @param value The  to remove.
	 */
	function Remove(value:Dynamic):Void;
	@:overload(function(index:Int):Void {})
	/**
	 * Removes the  object with the specified index from the collection.
	 * @param index The zero-based index of the  object to remove.
	 */
	function RemoveAt(sourceColumn:String):Void;
}
