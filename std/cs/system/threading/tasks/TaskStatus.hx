package cs.system.threading.tasks;

/** Represents the current stage in the lifecycle of a . */
@:native("System.Threading.Tasks.TaskStatus")
extern enum abstract TaskStatus(Int) {
	var Canceled = 6;
	var Created = 0;
	var Faulted = 7;
	var RanToCompletion = 5;
	var Running = 3;
	var WaitingForActivation = 1;
	var WaitingForChildrenToComplete = 4;
	var WaitingToRun = 2;
}
