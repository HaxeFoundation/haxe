package cs.system.transactions;

/** Specifies the isolation level of a transaction. */
@:native("System.Transactions.IsolationLevel")
extern enum IsolationLevel {
	Chaos;
	ReadCommitted;
	ReadUncommitted;
	RepeatableRead;
	Serializable;
	Snapshot;
	Unspecified;
}
