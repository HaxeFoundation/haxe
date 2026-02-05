package cs.system.transactions;

/** Describes the current status of a distributed transaction. */
@:native("System.Transactions.TransactionStatus")
extern enum abstract TransactionStatus(Int) {
	var Aborted = 2;
	var Active = 0;
	var Committed = 1;
	var InDoubt = 3;
}
