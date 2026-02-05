package cs.system.data.common;

/** Represents an SQL statement or stored procedure to execute against a data source. Provides a base class for database-specific classes that represent commands. */
@:native("System.Data.Common.DbCommand")
extern class DbCommand extends cs.system.componentmodel.Component {
	/**
	 * Gets or sets the text command to run against the data source.
	 * @return The text command to execute. The default value is an empty string ("").
	 */
	var CommandText(default, default):String;
	/**
	 * Gets or sets the wait time before terminating the attempt to execute a command
	 * and generating an error.
	 * @return The time in seconds to wait for the command to execute.
	 */
	var CommandTimeout(default, default):Int;
	/**
	 * Indicates or specifies how the  property is interpreted.
	 * @return One of the  values. The default is .
	 */
	var CommandType(default, default):cs.system.data.CommandType;
	/**
	 * Gets or sets the  used by this .
	 * @return The connection to the data source.
	 */
	var Connection(default, default):cs.system.data.common.DbConnection;
	/**
	 * Gets or sets the  used by this .
	 * @return The connection to the data source.
	 */
	var DbConnection(default, default):cs.system.data.common.DbConnection;
	/**
	 * Gets the collection of  objects.
	 * @return The parameters of the SQL statement or stored procedure.
	 */
	var DbParameterCollection(default, never):cs.system.data.common.DbParameterCollection;
	/**
	 * Gets or sets the  within which this  object executes.
	 * @return The transaction within which a Command object of a .NET Framework data
	 * provider executes. The default value is a null reference ( in Visual Basic).
	 */
	var DbTransaction(default, default):cs.system.data.common.DbTransaction;
	/**
	 * Gets or sets a value indicating whether the command object should be visible in
	 * a customized interface control.
	 * @return , if the command object should be visible in a control; otherwise . The
	 * default is .
	 */
	var DesignTimeVisible(default, default):Bool;
	/**
	 * Gets the collection of  objects. For more information on parameters, see
	 * Configuring Parameters and Parameter Data Types.
	 * @return The parameters of the SQL statement or stored procedure.
	 */
	var Parameters(default, never):cs.system.data.common.DbParameterCollection;
	/**
	 * Gets or sets the  within which this  object executes.
	 * @return The transaction within which a  object of a .NET Framework data provider
	 * executes. The default value is a null reference ( in Visual Basic).
	 */
	var Transaction(default, default):cs.system.data.common.DbTransaction;
	/**
	 * Gets or sets how command results are applied to the  when used by the Update
	 * method of a .
	 * @return One of the  values. The default is  unless the command is automatically
	 * generated. Then the default is .
	 */
	var UpdatedRowSource(default, default):cs.system.data.UpdateRowSource;
	/** Attempts to cancels the execution of a . */
	function Cancel():Void;
	/**
	 * Creates a new instance of a  object.
	 * @return A  object.
	 */
	function CreateParameter():cs.system.data.common.DbParameter;
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
	/**
	 * Executes a SQL statement against a connection object.
	 * @return The number of rows affected.
	 */
	function ExecuteNonQuery():Int;
	@:overload(function():cs.system.threading.tasks.Task_1<Int> {})
	/**
	 * An asynchronous version of , which executes a SQL statement against a connection
	 * object. Invokes  with CancellationToken.None.
	 * @return A task representing the asynchronous operation.
	 */
	function ExecuteNonQueryAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Int>;
	@:overload(function():cs.system.data.common.DbDataReader {})
	/**
	 * Executes the  against the , and returns an .
	 * @return A  object.
	 */
	function ExecuteReader(behavior:cs.system.data.CommandBehavior):cs.system.data.common.DbDataReader;
	@:overload(function():cs.system.threading.tasks.Task_1<cs.system.data.common.DbDataReader> {})
	@:overload(function(behavior:cs.system.data.CommandBehavior):cs.system.threading.tasks.Task_1<cs.system.data.common.DbDataReader> {})
	@:overload(function(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.data.common.DbDataReader> {})
	/**
	 * An asynchronous version of , which executes the  against the  and returns a .
	 * Invokes  with CancellationToken.None.
	 * @return A task representing the asynchronous operation.
	 */
	function ExecuteReaderAsync(behavior:cs.system.data.CommandBehavior, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.data.common.DbDataReader>;
	/**
	 * Executes the query and returns the first column of the first row in the result
	 * set returned by the query. All other columns and rows are ignored.
	 * @return The first column of the first row in the result set.
	 */
	function ExecuteScalar():Dynamic;
	@:overload(function():cs.system.threading.tasks.Task_1<Dynamic> {})
	/**
	 * An asynchronous version of , which executes the query and returns the first
	 * column of the first row in the result set returned by the query. All other
	 * columns and rows are ignored. Invokes  with CancellationToken.None.
	 * @return A task representing the asynchronous operation.
	 */
	function ExecuteScalarAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Dynamic>;
	/** Creates a prepared (or compiled) version of the command on the data source. */
	function Prepare():Void;
	function PrepareAsync(?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
}
