package cs.system.data;

/** Provides a means of reading one or more forward-only streams of result sets obtained by executing a command at a data source, and is implemented by .NET Framework data providers that access relational databases. */
@:native("System.Data.IDataReader")
extern interface IDataReader extends cs.system.data.IDataRecord extends cs.system.IDisposable {
	/**
	 * Gets a value indicating the depth of nesting for the current row.
	 * @return The level of nesting.
	 */
	var Depth(default, never):Int;
	/**
	 * Gets a value indicating whether the data reader is closed.
	 * @return if the data reader is closed; otherwise, .
	 */
	var IsClosed(default, never):Bool;
	/**
	 * Gets the number of rows changed, inserted, or deleted by execution of the SQL
	 * statement.
	 * @return The number of rows changed, inserted, or deleted; 0 if no rows were
	 * affected or the statement failed; and -1 for SELECT statements.
	 */
	var RecordsAffected(default, never):Int;
	/** Closes the  Object. */
	function Close():Void;
	/**
	 * Returns a  that describes the column metadata of the .
	 * @return A  that describes the column metadata.
	 */
	function GetSchemaTable():cs.system.data.DataTable;
	/**
	 * Advances the data reader to the next result, when reading the results of batch
	 * SQL statements.
	 * @return if there are more rows; otherwise, .
	 */
	function NextResult():Bool;
	/**
	 * Advances the  to the next record.
	 * @return if there are more rows; otherwise, .
	 */
	function Read():Bool;
}
