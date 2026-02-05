package cs.system.data.common;

/** Aids implementation of the  interface. Inheritors of  implement a set of functions to provide strong typing, but inherit most of the functionality needed to fully implement a DataAdapter. */
@:native("System.Data.Common.DbDataAdapter")
extern class DbDataAdapter extends cs.system.data.common.DataAdapter {
	/** The default name used by the  object for table mappings. */
	static var DefaultSourceTableName(default, never):String;
	/**
	 * Gets or sets a command for deleting records from the data set.
	 * @return An  used during  to delete records in the data source for deleted rows
	 * in the data set.
	 */
	var DeleteCommand(default, default):cs.system.data.common.DbCommand;
	/**
	 * Gets or sets the behavior of the command used to fill the data adapter.
	 * @return The  of the command used to fill the data adapter.
	 */
	var FillCommandBehavior(default, default):cs.system.data.CommandBehavior;
	/**
	 * Gets or sets a command used to insert new records into the data source.
	 * @return A  used during  to insert records in the data source for new rows in the
	 * data set.
	 */
	var InsertCommand(default, default):cs.system.data.common.DbCommand;
	/**
	 * Gets or sets a command used to select records in the data source.
	 * @return A  that is used during  to select records from data source for placement
	 * in the data set.
	 */
	var SelectCommand(default, default):cs.system.data.common.DbCommand;
	/**
	 * Gets or sets a value that enables or disables batch processing support, and
	 * specifies the number of commands that can be executed in a batch.
	 * @return The number of rows to process per batch. Value is Effect 0 There is no
	 * limit on the batch size. 1 Disables batch updating. > 1 Changes are sent using
	 * batches of  operations at a time. When setting this to a value other than 1, all
	 * the commands associated with the  must have their  property set to None or
	 * OutputParameters. An exception will be thrown otherwise.
	 */
	var UpdateBatchSize(default, default):Int;
	/**
	 * Gets or sets a command used to update records in the data source.
	 * @return A  used during  to update records in the data source for modified rows
	 * in the data set.
	 */
	var UpdateCommand(default, default):cs.system.data.common.DbCommand;
	@:overload(function(dataSet:cs.system.data.DataSet):Int {})
	@:overload(function(dataTable:cs.system.data.DataTable):Int {})
	@:overload(function(dataSet:cs.system.data.DataSet, srcTable:String):Int {})
	@:overload(function(startRecord:Int, maxRecords:Int, dataTables:cs.NativeArray<cs.system.data.DataTable>):Int {})
	/**
	 * Adds or refreshes rows in the .
	 * @param dataSet A  to fill with records and, if necessary, schema.
	 * @return The number of rows successfully added to or refreshed in the . This does
	 * not include rows affected by statements that do not return rows.
	 */
	function Fill(dataSet:cs.system.data.DataSet, startRecord:Int, maxRecords:Int, srcTable:String):Int;
	@:overload(function(dataSet:cs.system.data.DataSet, schemaType:cs.system.data.SchemaType):cs.NativeArray<cs.system.data.DataTable> {})
	@:overload(function(dataTable:cs.system.data.DataTable, schemaType:cs.system.data.SchemaType):cs.system.data.DataTable {})
	/**
	 * Adds a  named "Table" to the specified  and configures the schema to match that
	 * in the data source based on the specified .
	 * @param dataSet A  to insert the schema in.
	 * @param schemaType One of the  values that specify how to insert the schema.
	 * @return A reference to a collection of  objects that were added to the .
	 */
	function FillSchema(dataSet:cs.system.data.DataSet, schemaType:cs.system.data.SchemaType, srcTable:String):cs.NativeArray<cs.system.data.DataTable>;
	/**
	 * Gets the parameters set by the user when executing an SQL SELECT statement.
	 * @return An array of  objects that contains the parameters set by the user.
	 */
	function GetFillParameters():cs.NativeArray<cs.system.data.IDataParameter>;
	@:overload(function(dataRows:cs.NativeArray<cs.system.data.DataRow>):Int {})
	@:overload(function(dataSet:cs.system.data.DataSet):Int {})
	@:overload(function(dataTable:cs.system.data.DataTable):Int {})
	/**
	 * Updates the values in the database by executing the respective INSERT, UPDATE,
	 * or DELETE statements for each inserted, updated, or deleted row in the specified
	 * array in the .
	 * @param dataRows An array of  objects used to update the data source.
	 * @return The number of rows successfully updated from the .
	 */
	function Update(dataSet:cs.system.data.DataSet, srcTable:String):Int;
}
