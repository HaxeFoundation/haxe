package cs.system.data;

/** Represents an open connection to a data source, and is implemented by .NET Framework data providers that access relational databases. */
@:native("System.Data.IDbConnection")
extern interface IDbConnection extends cs.system.IDisposable {
	/**
	 * Gets or sets the string used to open a database.
	 * @return A string containing connection settings.
	 */
	var ConnectionString(default, default):String;
	/**
	 * Gets the time to wait while trying to establish a connection before terminating
	 * the attempt and generating an error.
	 * @return The time (in seconds) to wait for a connection to open. The default
	 * value is 15 seconds.
	 */
	var ConnectionTimeout(default, never):Int;
	/**
	 * Gets the name of the current database or the database to be used after a
	 * connection is opened.
	 * @return The name of the current database or the name of the database to be used
	 * once a connection is open. The default value is an empty string.
	 */
	var Database(default, never):String;
	/**
	 * Gets the current state of the connection.
	 * @return One of the  values.
	 */
	var State(default, never):cs.system.data.ConnectionState;
	@:overload(function():cs.system.data.IDbTransaction {})
	/**
	 * Begins a database transaction.
	 * @return An object representing the new transaction.
	 */
	function BeginTransaction(il:cs.system.data.IsolationLevel):cs.system.data.IDbTransaction;
	/**
	 * Changes the current database for an open  object.
	 * @param databaseName The name of the database to use in place of the current
	 * database.
	 */
	function ChangeDatabase(databaseName:String):Void;
	/** Closes the connection to the database. */
	function Close():Void;
	/**
	 * Creates and returns a Command object associated with the connection.
	 * @return A Command object associated with the connection.
	 */
	function CreateCommand():cs.system.data.IDbCommand;
	/** Opens a database connection with the settings specified by the  property of the provider-specific Connection object. */
	function Open():Void;
}
