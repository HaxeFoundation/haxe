package cs.system.data;

/** Specifies the transaction locking behavior for the connection. */
@:native("System.Data.IsolationLevel")
extern enum IsolationLevel {
	Chaos;
	ReadCommitted;
	ReadUncommitted;
	RepeatableRead;
	Serializable;
	Snapshot;
	Unspecified;
}
