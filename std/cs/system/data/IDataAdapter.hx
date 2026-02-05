package cs.system.data;

/** Allows an object to implement a DataAdapter, and represents a set of methods and mapping action-related properties that are used to fill and update a  and update a data source. instances are for data sources that are (or resemble) relational databases with textual commands (like Transact-SQL), while  instances could can use any type of data source. */
@:native("System.Data.IDataAdapter")
extern interface IDataAdapter {
	/**
	 * Indicates or specifies whether unmapped source tables or columns are passed with
	 * their source names in order to be filtered or to raise an error.
	 * @return One of the  values. The default is .
	 */
	var MissingMappingAction(default, default):cs.system.data.MissingMappingAction;
	/**
	 * Indicates or specifies whether missing source tables, columns, and their
	 * relationships are added to the dataset schema, ignored, or cause an error to be
	 * raised.
	 * @return One of the  values. The default is .
	 */
	var MissingSchemaAction(default, default):cs.system.data.MissingSchemaAction;
	/**
	 * Indicates how a source table is mapped to a dataset table.
	 * @return A collection that provides the master mapping between the returned
	 * records and the . The default value is an empty collection.
	 */
	var TableMappings(default, never):cs.system.data.ITableMappingCollection;
	/**
	 * Adds or updates rows in the  to match those in the data source using the  name,
	 * and creates a  named "Table".
	 * @param dataSet A  to fill with records and, if necessary, schema.
	 * @return The number of rows successfully added to or refreshed in the . This does
	 * not include rows affected by statements that do not return rows.
	 */
	function Fill(dataSet:cs.system.data.DataSet):Int;
	/**
	 * Adds a  named "Table" to the specified  and configures the schema to match that
	 * in the data source based on the specified .
	 * @param dataSet The  to be filled with the schema from the data source.
	 * @param schemaType One of the  values.
	 * @return An array of  objects that contain schema information returned from the
	 * data source.
	 */
	function FillSchema(dataSet:cs.system.data.DataSet, schemaType:cs.system.data.SchemaType):cs.NativeArray<cs.system.data.DataTable>;
	/**
	 * Gets the parameters set by the user when executing an SQL SELECT statement.
	 * @return An array of  objects that contains the parameters set by the user.
	 */
	function GetFillParameters():cs.NativeArray<cs.system.data.IDataParameter>;
	/**
	 * Calls the respective INSERT, UPDATE, or DELETE statements for each inserted,
	 * updated, or deleted row in the specified  from a  named "Table".
	 * @param dataSet The  used to update the data source.
	 * @return The number of rows successfully updated from the .
	 */
	function Update(dataSet:cs.system.data.DataSet):Int;
}
