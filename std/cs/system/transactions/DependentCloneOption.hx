package cs.system.transactions;

/** Controls what kind of dependent transaction to create. */
@:native("System.Transactions.DependentCloneOption")
extern enum abstract DependentCloneOption(Int) {
	var BlockCommitUntilComplete = 0;
	var RollbackIfNotComplete = 1;
}
