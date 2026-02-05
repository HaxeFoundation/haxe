package cs.system.data.common;

/** Contains a description of a mapped relationship between a source table and a . This class is used by a  when populating a . */
@:native("System.Data.Common.DataTableMapping")
extern class DataTableMapping extends cs.system.MarshalByRefObject {
	/**
	 * Gets the  for the .
	 * @return A data column mapping collection.
	 */
	var ColumnMappings(default, never):cs.system.data.common.DataColumnMappingCollection;
	/**
	 * Gets or sets the table name from a .
	 * @return The table name from a .
	 */
	var DataSetTable(default, default):String;
	/**
	 * Gets or sets the case-sensitive source table name from a data source.
	 * @return The case-sensitive source table name from a data source.
	 */
	var SourceTable(default, default):String;
	@:overload(function():Void {})
	@:overload(function(sourceTable:String, dataSetTable:String):Void {})
	function new(sourceTable:String, dataSetTable:String, columnMappings:cs.NativeArray<cs.system.data.common.DataColumnMapping>):Void;
	/**
	 * Gets a  from the specified  using the specified  value and the name of the .
	 * @param sourceColumn The name of the .
	 * @param mappingAction One of the  values.
	 * @return A data column.
	 */
	function GetColumnMappingBySchemaAction(sourceColumn:String, mappingAction:cs.system.data.MissingMappingAction):cs.system.data.common.DataColumnMapping;
	/**
	 * Returns a  object for a given column name.
	 * @param sourceColumn The name of the .
	 * @param dataType The data type for .
	 * @param dataTable The table name from a  to map to.
	 * @param mappingAction One of the  values.
	 * @param schemaAction One of the  values.
	 * @return A  object.
	 */
	function GetDataColumn(sourceColumn:String, dataType:cs.system.Type, dataTable:cs.system.data.DataTable, mappingAction:cs.system.data.MissingMappingAction, schemaAction:cs.system.data.MissingSchemaAction):cs.system.data.DataColumn;
	/**
	 * Gets the current  for the specified  using the specified  value.
	 * @param dataSet The  from which to get the .
	 * @param schemaAction One of the  values.
	 * @return A data table.
	 */
	function GetDataTableBySchemaAction(dataSet:cs.system.data.DataSet, schemaAction:cs.system.data.MissingSchemaAction):cs.system.data.DataTable;
	/**
	 * Converts the current  name to a string.
	 * @return The current  name, as a string.
	 */
	function ToString():String;
}
