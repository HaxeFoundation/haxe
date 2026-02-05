package cs.system.data.common;

/** Represents a connection to a database. */
@:native("System.Data.Common.DbConnection")
extern class DbConnection extends cs.system.componentmodel.Component {
	/**
	 * Gets or sets the string used to open the connection.
	 * @return The connection string used to establish the initial connection. The
	 * exact contents of the connection string depend on the specific data source for
	 * this connection. The default value is an empty string.
	 */
	var ConnectionString(default, default):String;
	/**
	 * Gets the time to wait while establishing a connection before terminating the
	 * attempt and generating an error.
	 * @return The time (in seconds) to wait for a connection to open. The default
	 * value is determined by the specific type of connection that you are using.
	 */
	var ConnectionTimeout(default, never):Int;
	/**
	 * Gets the name of the current database after a connection is opened, or the
	 * database name specified in the connection string before the connection is
	 * opened.
	 * @return The name of the current database or the name of the database to be used
	 * after a connection is opened. The default value is an empty string.
	 */
	var Database(default, never):String;
	/**
	 * Gets the name of the database server to which to connect.
	 * @return The name of the database server to which to connect. The default value
	 * is an empty string.
	 */
	var DataSource(default, never):String;
	/**
	 * Gets the  for this .
	 * @return A set of methods for creating instances of a provider's implementation
	 * of the data source classes.
	 */
	var DbProviderFactory(default, never):cs.system.data.common.DbProviderFactory;
	/**
	 * Gets a string that represents the version of the server to which the object is
	 * connected.
	 * @return The version of the database. The format of the string returned depends
	 * on the specific type of connection you are using.
	 */
	var ServerVersion(default, never):String;
	/**
	 * Gets a string that describes the state of the connection.
	 * @return The state of the connection. The format of the string returned depends
	 * on the specific type of connection you are using.
	 */
	var State(default, never):cs.system.data.ConnectionState;
	@:overload(function():cs.system.data.common.DbTransaction {})
	/**
	 * Starts a database transaction.
	 * @return An object representing the new transaction.
	 */
	function BeginTransaction(isolationLevel:cs.system.data.IsolationLevel):cs.system.data.common.DbTransaction;
	@:overload(function(?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<cs.system.data.common.DbTransaction> {})
	function BeginTransactionAsync(isolationLevel:cs.system.data.IsolationLevel, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<cs.system.data.common.DbTransaction>;
	/**
	 * Changes the current database for an open connection.
	 * @param databaseName Specifies the name of the database for the connection to
	 * use.
	 */
	function ChangeDatabase(databaseName:String):Void;
	function ChangeDatabaseAsync(databaseName:String, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/** Closes the connection to the database. This is the preferred method of closing any open connection. */
	function Close():Void;
	function CloseAsync():cs.system.threading.tasks.Task;
	/**
	 * Creates and returns a  object associated with the current connection.
	 * @return A  object.
	 */
	function CreateCommand():cs.system.data.common.DbCommand;
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
	/**
	 * Enlists in the specified transaction.
	 * @param transaction A reference to an existing  in which to enlist.
	 */
	function EnlistTransaction(transaction:cs.system.transactions.Transaction):Void;
	@:overload(function():cs.system.data.DataTable {})
	@:overload(function(collectionName:String):cs.system.data.DataTable {})
	/**
	 * Returns schema information for the data source of this .
	 * @return A  that contains schema information.
	 */
	function GetSchema(collectionName:String, restrictionValues:cs.NativeArray<String>):cs.system.data.DataTable;
	/** Opens a database connection with the settings specified by the . */
	function Open():Void;
	@:overload(function():cs.system.threading.tasks.Task {})
	/**
	 * An asynchronous version of , which opens a database connection with the settings
	 * specified by the . This method invokes the virtual method  with
	 * CancellationToken.None.
	 * @return A task representing the asynchronous operation.
	 */
	function OpenAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
}
