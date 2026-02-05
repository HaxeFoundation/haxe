package cs.system.transactions;

/** Describes a DTC transaction. */
@:native("System.Transactions.IDtcTransaction")
extern interface IDtcTransaction {
	/**
	 * Aborts a transaction.
	 * @param reason An optional  that indicates why the transaction is being aborted.
	 * This parameter can be , indicating that no reason for the abort is provided.
	 * @param retaining This value must be .
	 * @param async When  is , an asynchronous abort is performed and the caller must
	 * use  to learn about the outcome of the transaction.
	 */
	function Abort(reason:cs.system.IntPtr, retaining:Int, async:Int):Void;
	/**
	 * Commits a transaction.
	 * @param retaining This value must be .
	 * @param commitType A value taken from the OLE DB enumeration .
	 * @param reserved This value must be zero.
	 */
	function Commit(retaining:Int, commitType:Int, reserved:Int):Void;
	/**
	 * Retrieves information about a transaction.
	 * @param transactionInformation Pointer to the caller-allocated  structure that
	 * will receive information about the transaction. This value must not be .
	 */
	function GetTransactionInfo(transactionInformation:cs.system.IntPtr):Void;
}
