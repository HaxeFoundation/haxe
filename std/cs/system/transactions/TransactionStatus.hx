package cs.system.transactions;

/** Describes the current status of a distributed transaction. */
@:native("System.Transactions.TransactionStatus")
extern enum TransactionStatus {
	Aborted;
	Active;
	Committed;
	InDoubt;
}
