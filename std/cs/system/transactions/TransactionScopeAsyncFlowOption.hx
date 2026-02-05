package cs.system.transactions;

/** Specifies whether transaction flow across thread continuations is enabled for . */
@:native("System.Transactions.TransactionScopeAsyncFlowOption")
extern enum abstract TransactionScopeAsyncFlowOption(Int) {
	var Enabled = 1;
	var Suppress = 0;
}
