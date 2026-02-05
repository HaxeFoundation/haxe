package cs.system.data;

/** Represents an SQL statement that is executed while connected to a data source, and is implemented by .NET Framework data providers that access relational databases. */
@:native("System.Data.IDbCommand")
extern interface IDbCommand extends cs.system.IDisposable {
	/**
	 * Gets or sets the text command to run against the data source.
	 * @return The text command to execute. The default value is an empty string ("").
	 */
	var CommandText(default, default):String;
	/**
	 * Gets or sets the wait time before terminating the attempt to execute a command
	 * and generating an error.
	 * @return The time (in seconds) to wait for the command to execute. The default
	 * value is 30 seconds.
	 */
	var CommandTimeout(default, default):Int;
	/**
	 * Indicates or specifies how the  property is interpreted.
	 * @return One of the  values. The default is .
	 */
	var CommandType(default, default):cs.system.data.CommandType;
	/**
	 * Gets or sets the  used by this instance of the .
	 * @return The connection to the data source.
	 */
	var Connection(default, default):cs.system.data.IDbConnection;
	/**
	 * Gets the .
	 * @return The parameters of the SQL statement or stored procedure.
	 */
	var Parameters(default, never):cs.system.data.IDataParameterCollection;
	/**
	 * Gets or sets the transaction within which the  object of a .NET Framework data
	 * provider executes.
	 * @return the  object of a .NET Framework data provider executes. The default
	 * value is .
	 */
	var Transaction(default, default):cs.system.data.IDbTransaction;
	/**
	 * Gets or sets how command results are applied to the  when used by the  method of
	 * a .
	 * @return One of the  values. The default is  unless the command is automatically
	 * generated. Then the default is .
	 */
	var UpdatedRowSource(default, default):cs.system.data.UpdateRowSource;
	/** Attempts to cancels the execution of an . */
	function Cancel():Void;
	/**
	 * Creates a new instance of an  object.
	 * @return An  object.
	 */
	function CreateParameter():cs.system.data.IDbDataParameter;
	/**
	 * Executes an SQL statement against the  object of a .NET Framework data provider,
	 * and returns the number of rows affected.
	 * @return The number of rows affected.
	 */
	function ExecuteNonQuery():Int;
	@:overload(function():cs.system.data.IDataReader {})
	/**
	 * Executes the  against the  and builds an .
	 * @return An  object.
	 */
	function ExecuteReader(behavior:cs.system.data.CommandBehavior):cs.system.data.IDataReader;
	/**
	 * Executes the query, and returns the first column of the first row in the
	 * resultset returned by the query. Extra columns or rows are ignored.
	 * @return The first column of the first row in the resultset.
	 */
	function ExecuteScalar():Dynamic;
	/** Creates a prepared (or compiled) version of the command on the data source. */
	function Prepare():Void;
}
