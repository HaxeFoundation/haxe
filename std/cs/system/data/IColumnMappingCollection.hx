package cs.system.data;

/** Contains a collection of DataColumnMapping objects, and is implemented by the , which is used in common by .NET Framework data providers. */
@:native("System.Data.IColumnMappingCollection")
extern interface IColumnMappingCollection extends cs.system.collections.ICollection extends cs.system.collections.IEnumerable extends cs.system.collections.IList {
	@:native("get_Item")
	function get_Item(index0:String):Dynamic;
	@:native("set_Item")
	function set_Item(index0:String, value:Dynamic):Void;
	/**
	 * Adds a ColumnMapping object to the ColumnMapping collection using the source
	 * column and  column names.
	 * @param sourceColumnName The case-sensitive name of the source column.
	 * @param dataSetColumnName The name of the  column.
	 * @return The ColumnMapping object that was added to the collection.
	 */
	function Add(sourceColumnName:String, dataSetColumnName:String):cs.system.data.IColumnMapping;
	/**
	 * Gets a value indicating whether the  contains a  object with the specified
	 * source column name.
	 * @param sourceColumnName The case-sensitive name of the source column.
	 * @return if a  object with the specified source column name exists, otherwise .
	 */
	function Contains(sourceColumnName:String):Bool;
	/**
	 * Gets the ColumnMapping object with the specified  column name.
	 * @param dataSetColumnName The name of the  column within the collection.
	 * @return The ColumnMapping object with the specified  column name.
	 */
	function GetByDataSetColumn(dataSetColumnName:String):cs.system.data.IColumnMapping;
	/**
	 * Gets the location of the  object with the specified source column name. The name
	 * is case-sensitive.
	 * @param sourceColumnName The case-sensitive name of the source column.
	 * @return The zero-based location of the  object with the specified source column
	 * name.
	 */
	function IndexOf(sourceColumnName:String):Int;
	/**
	 * Removes the  object with the specified  name from the collection.
	 * @param sourceColumnName The case-sensitive  name.
	 */
	function RemoveAt(sourceColumnName:String):Void;
}
