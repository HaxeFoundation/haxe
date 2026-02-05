package cs.system.threading.tasks.sources;

/** Indicates the status of an  or . */
@:native("System.Threading.Tasks.Sources.ValueTaskSourceStatus")
extern enum abstract ValueTaskSourceStatus(Int) {
	var Canceled = 3;
	var Faulted = 2;
	var Pending = 0;
	var Succeeded = 1;
}
