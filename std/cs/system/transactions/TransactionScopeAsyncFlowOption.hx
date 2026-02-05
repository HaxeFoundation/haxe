package cs.system.transactions;

/** Specifies whether transaction flow across thread continuations is enabled for . */
@:native("System.Transactions.TransactionScopeAsyncFlowOption")
extern enum TransactionScopeAsyncFlowOption {
	Enabled;
	Suppress;
}
