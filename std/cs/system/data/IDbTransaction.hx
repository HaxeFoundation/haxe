package cs.system.data;

/** Represents a transaction to be performed at a data source, and is implemented by .NET Framework data providers that access relational databases. */
@:native("System.Data.IDbTransaction")
extern interface IDbTransaction extends cs.system.IDisposable {
	/**
	 * Specifies the Connection object to associate with the transaction.
	 * @return The Connection object to associate with the transaction.
	 */
	var Connection(default, never):cs.system.data.IDbConnection;
	/**
	 * Specifies the  for this transaction.
	 * @return The  for this transaction. The default is .
	 */
	var IsolationLevel(default, never):cs.system.data.IsolationLevel;
	/** Commits the database transaction. */
	function Commit():Void;
	/** Rolls back a transaction from a pending state. */
	function Rollback():Void;
}
