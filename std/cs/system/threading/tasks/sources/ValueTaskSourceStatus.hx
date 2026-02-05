package cs.system.threading.tasks.sources;

/** Indicates the status of an  or . */
@:native("System.Threading.Tasks.Sources.ValueTaskSourceStatus")
extern enum ValueTaskSourceStatus {
	Canceled;
	Faulted;
	Pending;
	Succeeded;
}
