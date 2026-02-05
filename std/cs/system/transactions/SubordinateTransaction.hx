package cs.system.transactions;

/** Represents a non-rooted transaction that can be delegated. This class cannot be inherited. */
@:native("System.Transactions.SubordinateTransaction")
extern class SubordinateTransaction extends cs.system.transactions.Transaction {
	function new(isoLevel:cs.system.transactions.IsolationLevel, superior:cs.system.transactions.ISimpleTransactionSuperior):Void;
}
