package cs.system.transactions;

/** Describes a committable transaction. */
@:native("System.Transactions.CommittableTransaction")
extern class CommittableTransaction extends cs.system.transactions.Transaction {
	@:overload(function():Void {})
	@:overload(function(timeout:cs.system.TimeSpan):Void {})
	function new(options:cs.system.transactions.TransactionOptions):Void;
	/**
	 * Begins an attempt to commit the transaction asynchronously.
	 * @param asyncCallback The  delegate that is invoked when the transaction
	 * completes. This parameter can be , in which case the application is not notified
	 * of the transaction's completion. Instead, the application must use the 
	 * interface to check for completion and wait accordingly, or call  to wait for
	 * completion.
	 * @param asyncState An object, which might contain arbitrary state information,
	 * associated with the asynchronous commitment. This object is passed to the
	 * callback, and is not interpreted by . A null reference is permitted.
	 * @return An  interface that can be used by the caller to check the status of the
	 * asynchronous operation, or to wait for the operation to complete.
	 */
	function BeginCommit(asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult;
	/** Attempts to commit the transaction. */
	function Commit():Void;
	/**
	 * Ends an attempt to commit the transaction asynchronously.
	 * @param asyncResult The  object associated with the asynchronous commitment.
	 */
	function EndCommit(asyncResult:cs.system.IAsyncResult):Void;
}
