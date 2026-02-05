package cs.system.transactions;

/** Provides additional options for creating a transaction scope. */
@:native("System.Transactions.TransactionScopeOption")
extern enum TransactionScopeOption {
	Required;
	RequiresNew;
	Suppress;
}
