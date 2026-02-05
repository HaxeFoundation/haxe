package cs.system.transactions;

/** Represents a transaction that is not a root transaction, but can be escalated to be managed by the MSDTC. */
@:native("System.Transactions.ISimpleTransactionSuperior")
extern interface ISimpleTransactionSuperior extends cs.system.transactions.ITransactionPromoter {
	/** Notifies an enlisted object that the transaction is being rolled back. */
	function Rollback():Void;
}
