package cs.system.data.common;

/** Represents a set of SQL commands and a database connection that are used to fill the  and update the data source. */
@:native("System.Data.Common.DataAdapter")
extern class DataAdapter extends cs.system.componentmodel.Component {
	/**
	 * Gets or sets a value indicating whether  is called on a  after it is added to
	 * the  during any of the Fill operations.
	 * @return if  is called on the ; otherwise . The default is .
	 */
	var AcceptChangesDuringFill(default, default):Bool;
	/**
	 * Gets or sets whether  is called during a .
	 * @return if  is called during an ; otherwise . The default is .
	 */
	var AcceptChangesDuringUpdate(default, default):Bool;
	/**
	 * Gets or sets a value that specifies whether to generate an exception when an
	 * error is encountered during a row update.
	 * @return to continue the update without generating an exception; otherwise . The
	 * default is .
	 */
	var ContinueUpdateOnError(default, default):Bool;
	/**
	 * Gets or sets the  that determines how the adapter fills the  from the .
	 * @return A  value.
	 */
	var FillLoadOption(default, default):cs.system.data.LoadOption;
	/**
	 * Determines the action to take when incoming data does not have a matching table
	 * or column.
	 * @return One of the  values. The default is .
	 */
	var MissingMappingAction(default, default):cs.system.data.MissingMappingAction;
	/**
	 * Determines the action to take when existing  schema does not match incoming
	 * data.
	 * @return One of the  values. The default is .
	 */
	var MissingSchemaAction(default, default):cs.system.data.MissingSchemaAction;
	/**
	 * Gets or sets whether the  method should return provider-specific values or
	 * common CLS-compliant values.
	 * @return if the  method should return provider-specific values; otherwise  to
	 * return common CLS-compliant values.
	 */
	var ReturnProviderSpecificTypes(default, default):Bool;
	/**
	 * Gets a collection that provides the master mapping between a source table and a
	 * .
	 * @return A collection that provides the master mapping between the returned
	 * records and the . The default value is an empty collection.
	 */
	var TableMappings(default, never):cs.system.data.common.DataTableMappingCollection;
	/**
	 * Adds or refreshes rows in the  to match those in the data source.
	 * @param dataSet A  to fill with records and, if necessary, schema.
	 * @return The number of rows successfully added to or refreshed in the . This does
	 * not include rows affected by statements that do not return rows.
	 */
	function Fill(dataSet:cs.system.data.DataSet):Int;
	/**
	 * Adds a  to the specified  and configures the schema to match that in the data
	 * source based on the specified .
	 * @param dataSet The  to be filled with the schema from the data source.
	 * @param schemaType One of the  values.
	 * @return A  object that contains schema information returned from the data
	 * source.
	 */
	function FillSchema(dataSet:cs.system.data.DataSet, schemaType:cs.system.data.SchemaType):cs.NativeArray<cs.system.data.DataTable>;
	/**
	 * Gets the parameters set by the user when executing an SQL SELECT statement.
	 * @return An array of  objects that contains the parameters set by the user.
	 */
	function GetFillParameters():cs.NativeArray<cs.system.data.IDataParameter>;
	/** Resets  to its default state and causes  to honor . */
	function ResetFillLoadOption():Void;
	/**
	 * Determines whether the  property should be persisted.
	 * @return if the  property is persisted; otherwise .
	 */
	function ShouldSerializeAcceptChangesDuringFill():Bool;
	/**
	 * Determines whether the  property should be persisted.
	 * @return if the  property is persisted; otherwise .
	 */
	function ShouldSerializeFillLoadOption():Bool;
	/**
	 * Calls the respective INSERT, UPDATE, or DELETE statements for each inserted,
	 * updated, or deleted row in the specified  from a  named "Table."
	 * @param dataSet The  used to update the data source.
	 * @return The number of rows successfully updated from the .
	 */
	function Update(dataSet:cs.system.data.DataSet):Int;
}
