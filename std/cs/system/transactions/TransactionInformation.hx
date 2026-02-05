package cs.system.transactions;

/** Provides additional information regarding a transaction. */
@:native("System.Transactions.TransactionInformation")
extern class TransactionInformation {
	/**
	 * Gets the creation time of the transaction.
	 * @return A  that contains the creation time of the transaction.
	 */
	var CreationTime(default, never):cs.system.DateTime;
	/**
	 * Gets a unique identifier of the escalated transaction.
	 * @return A  that contains the unique identifier of the escalated transaction.
	 */
	var DistributedIdentifier(default, never):cs.system.Guid;
	/**
	 * Gets a unique identifier of the transaction.
	 * @return A unique identifier of the transaction.
	 */
	var LocalIdentifier(default, never):String;
	/**
	 * Gets the status of the transaction.
	 * @return A  that contains the status of the transaction.
	 */
	var Status(default, never):cs.system.transactions.TransactionStatus;
}
