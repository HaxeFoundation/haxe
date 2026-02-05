package cs.system.transactions;

/** Controls what kind of dependent transaction to create. */
@:native("System.Transactions.DependentCloneOption")
extern enum DependentCloneOption {
	BlockCommitUntilComplete;
	RollbackIfNotComplete;
}
