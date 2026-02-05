package cs.system.data;

/** Specifies the transaction locking behavior for the connection. */
@:native("System.Data.IsolationLevel")
extern enum abstract IsolationLevel(Int) {
	var Chaos = 16;
	var ReadCommitted = 4096;
	var ReadUncommitted = 256;
	var RepeatableRead = 65536;
	var Serializable = 1048576;
	var Snapshot = 16777216;
	var Unspecified = -1;
}
