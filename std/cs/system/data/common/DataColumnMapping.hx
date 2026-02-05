package cs.system.data.common;

/** Contains a generic column mapping for an object that inherits from . This class cannot be inherited. */
@:native("System.Data.Common.DataColumnMapping")
extern class DataColumnMapping extends cs.system.MarshalByRefObject {
	/**
	 * Gets or sets the name of the column within the  to map to.
	 * @return The name of the column within the  to map to. The name is not case
	 * sensitive.
	 */
	var DataSetColumn(default, default):String;
	/**
	 * Gets or sets the name of the column within the data source to map from. The name
	 * is case-sensitive.
	 * @return The case-sensitive name of the column in the data source.
	 */
	var SourceColumn(default, default):String;
	@:overload(function():Void {})
	function new(sourceColumn:String, dataSetColumn:String):Void;
	/**
	 * Gets a  from the given  using the  and the  property.
	 * @param dataTable The  to get the column from.
	 * @param dataType The  of the data column.
	 * @param schemaAction One of the  values.
	 * @return A data column.
	 */
	static function GetDataColumnBySchemaAction(sourceColumn:String, dataSetColumn:String, dataTable:cs.system.data.DataTable, dataType:cs.system.Type, schemaAction:cs.system.data.MissingSchemaAction):cs.system.data.DataColumn;
	/**
	 * Gets a  from the given  using the  and the  property.
	 * @param dataTable The  to get the column from.
	 * @param dataType The  of the data column.
	 * @param schemaAction One of the  values.
	 * @return A data column.
	 */
	function GetDataColumnBySchemaAction(dataTable:cs.system.data.DataTable, dataType:cs.system.Type, schemaAction:cs.system.data.MissingSchemaAction):cs.system.data.DataColumn;
	/**
	 * Converts the current  name to a string.
	 * @return The current  name as a string.
	 */
	function ToString():String;
}
