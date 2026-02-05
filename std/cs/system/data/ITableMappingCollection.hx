package cs.system.data;

/** Contains a collection of TableMapping objects, and is implemented by the , which is used in common by .NET Framework data providers. */
@:native("System.Data.ITableMappingCollection")
extern interface ITableMappingCollection extends cs.system.collections.ICollection extends cs.system.collections.IEnumerable extends cs.system.collections.IList {
	@:native("get_Item")
	function get_Item(index0:String):Dynamic;
	@:native("set_Item")
	function set_Item(index0:String, value:Dynamic):Void;
	/**
	 * Adds a table mapping to the collection.
	 * @param sourceTableName The case-sensitive name of the source table.
	 * @param dataSetTableName The name of the  table.
	 * @return A reference to the newly-mapped  object.
	 */
	function Add(sourceTableName:String, dataSetTableName:String):cs.system.data.ITableMapping;
	/**
	 * Gets a value indicating whether the collection contains a table mapping with the
	 * specified source table name.
	 * @param sourceTableName The case-sensitive name of the source table.
	 * @return if a table mapping with the specified source table name exists,
	 * otherwise .
	 */
	function Contains(sourceTableName:String):Bool;
	/**
	 * Gets the TableMapping object with the specified  table name.
	 * @param dataSetTableName The name of the  table within the collection.
	 * @return The TableMapping object with the specified  table name.
	 */
	function GetByDataSetTable(dataSetTableName:String):cs.system.data.ITableMapping;
	/**
	 * Gets the location of the  object within the collection.
	 * @param sourceTableName The case-sensitive name of the source table.
	 * @return The zero-based location of the  object within the collection.
	 */
	function IndexOf(sourceTableName:String):Int;
	/**
	 * Removes the  object with the specified  name from the collection.
	 * @param sourceTableName The case-sensitive name of the .
	 */
	function RemoveAt(sourceTableName:String):Void;
}
