package cs.system.transactions;

/** Specifies the isolation level of a transaction. */
@:native("System.Transactions.IsolationLevel")
extern enum abstract IsolationLevel(Int) {
	var Chaos = 5;
	var ReadCommitted = 2;
	var ReadUncommitted = 3;
	var RepeatableRead = 1;
	var Serializable = 0;
	var Snapshot = 4;
	var Unspecified = 6;
}
