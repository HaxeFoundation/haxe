package cs.system.transactions;

/** Describes a clone of a transaction providing guarantee that the transaction cannot be committed until the application comes to rest regarding work on the transaction. This class cannot be inherited. */
@:native("System.Transactions.DependentTransaction")
extern class DependentTransaction extends cs.system.transactions.Transaction {
	/** Attempts to complete the dependent transaction. */
	function Complete():Void;
}
