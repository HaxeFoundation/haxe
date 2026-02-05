package cs.system.data.common;

/** The base class for a transaction. */
@:native("System.Data.Common.DbTransaction")
extern class DbTransaction extends cs.system.MarshalByRefObject {
	/**
	 * Specifies the  object associated with the transaction.
	 * @return The  object associated with the transaction.
	 */
	var Connection(default, never):cs.system.data.common.DbConnection;
	/**
	 * Specifies the  object associated with the transaction.
	 * @return The  object associated with the transaction.
	 */
	var DbConnection(default, never):cs.system.data.common.DbConnection;
	/**
	 * Specifies the  for this transaction.
	 * @return The  for this transaction.
	 */
	var IsolationLevel(default, never):cs.system.data.IsolationLevel;
	/** Commits the database transaction. */
	function Commit():Void;
	function CommitAsync(?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/** Releases the unmanaged resources used by the . */
	function Dispose():Void;
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
	/** Rolls back a transaction from a pending state. */
	function Rollback():Void;
	function RollbackAsync(?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
}
