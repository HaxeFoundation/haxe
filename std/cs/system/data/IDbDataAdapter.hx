package cs.system.data;

/** Represents a set of command-related properties that are used to fill the  and update a data source, and is implemented by .NET Framework data providers that access relational databases. */
@:native("System.Data.IDbDataAdapter")
extern interface IDbDataAdapter extends cs.system.data.IDataAdapter {
	/**
	 * Gets or sets an SQL statement for deleting records from the data set.
	 * @return An  used during  to delete records in the data source for deleted rows
	 * in the data set.
	 */
	var DeleteCommand(default, default):cs.system.data.IDbCommand;
	/**
	 * Gets or sets an SQL statement used to insert new records into the data source.
	 * @return An  used during  to insert records in the data source for new rows in
	 * the data set.
	 */
	var InsertCommand(default, default):cs.system.data.IDbCommand;
	/**
	 * Gets or sets an SQL statement used to select records in the data source.
	 * @return An  that is used during  to select records from data source for
	 * placement in the data set.
	 */
	var SelectCommand(default, default):cs.system.data.IDbCommand;
	/**
	 * Gets or sets an SQL statement used to update records in the data source.
	 * @return An  used during  to update records in the data source for modified rows
	 * in the data set.
	 */
	var UpdateCommand(default, default):cs.system.data.IDbCommand;
}
