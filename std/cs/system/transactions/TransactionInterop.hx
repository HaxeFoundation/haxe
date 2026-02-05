package cs.system.transactions;

/** Facilitates interaction between  and components that were previously written to interact with MSDTC, COM+, or . This class cannot be inherited. */
@:native("System.Transactions.TransactionInterop")
extern class TransactionInterop {
	/** The type of the distributed transaction processor. */
	static var PromoterTypeDtc(default, never):cs.system.Guid;
	/**
	 * Gets an  instance that represents a .
	 * @param transaction A  instance to be marshaled.
	 * @return An  instance that represents a .  The  instance is compatible with the
	 * unmanaged form of ITransaction used by MSDTC and with the Managed form of  used
	 * by .
	 */
	static function GetDtcTransaction(transaction:cs.system.transactions.Transaction):cs.system.transactions.IDtcTransaction;
	/**
	 * Transforms a transaction object into an export transaction cookie.
	 * @param transaction The  object to be marshaled.
	 * @param whereabouts An address that describes the location of the destination
	 * transaction manager. This permits two transaction managers to communicate with
	 * one another and thereby propagate a transaction from one system to the other.
	 * @return An export transaction cookie representing the specified  object.
	 */
	static function GetExportCookie(transaction:cs.system.transactions.Transaction, whereabouts:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	/**
	 * Generates a  from a specified .
	 * @param transactionNative The  object to be marshaled.
	 * @return A  instance that represents the given .
	 */
	static function GetTransactionFromDtcTransaction(transactionNative:cs.system.transactions.IDtcTransaction):cs.system.transactions.Transaction;
	/**
	 * Generates a  from the specified an export cookie.
	 * @param cookie A marshaled form of the transaction object.
	 * @return A  from the specified export cookie.
	 */
	static function GetTransactionFromExportCookie(cookie:cs.NativeArray<cs.UInt8>):cs.system.transactions.Transaction;
	/**
	 * Generates a  instance from the specified transmitter propagation token.
	 * @param propagationToken A propagation token representing a transaction.
	 * @return A  from the specified transmitter propagation token.
	 */
	static function GetTransactionFromTransmitterPropagationToken(propagationToken:cs.NativeArray<cs.UInt8>):cs.system.transactions.Transaction;
	/**
	 * Generates a propagation token for the specified .
	 * @param transaction A transaction to be marshaled into a propagation token.
	 * @return This method, together with the  method, provide functionality for
	 * Transmitter/Receiver propagation, in which the transaction is "pulled" from the
	 * remote machine when the latter is called to unmarshal the transaction. For more
	 * information on different propagation models, see  class.
	 */
	static function GetTransmitterPropagationToken(transaction:cs.system.transactions.Transaction):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets the Whereabouts of the distributed transaction manager that  uses.
	 * @return The Whereabouts of the distributed transaction manager that  uses.
	 */
	static function GetWhereabouts():cs.NativeArray<cs.UInt8>;
}
