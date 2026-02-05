package cs.system.transactions;

/** Provides additional options for creating a transaction scope. */
@:native("System.Transactions.TransactionScopeOption")
extern enum abstract TransactionScopeOption(Int) {
	var Required = 0;
	var RequiresNew = 1;
	var Suppress = 2;
}
