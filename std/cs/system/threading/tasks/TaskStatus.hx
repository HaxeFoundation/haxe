package cs.system.threading.tasks;

/** Represents the current stage in the lifecycle of a . */
@:native("System.Threading.Tasks.TaskStatus")
extern enum TaskStatus {
	Canceled;
	Created;
	Faulted;
	RanToCompletion;
	Running;
	WaitingForActivation;
	WaitingForChildrenToComplete;
	WaitingToRun;
}
