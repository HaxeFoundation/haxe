package cs.system.threading.tasks;

/** Specifies flags that control optional behavior for the creation and execution of tasks. */
@:native("System.Threading.Tasks.TaskCreationOptions")
extern enum abstract TaskCreationOptions(Int) {
	var AttachedToParent = 4;
	var DenyChildAttach = 8;
	var HideScheduler = 16;
	var LongRunning = 2;
	var None = 0;
	var PreferFairness = 1;
	var RunContinuationsAsynchronously = 64;
	@:op(A | B) static function or(lhs:TaskCreationOptions, rhs:TaskCreationOptions):TaskCreationOptions;
	@:op(A & B) static function and(lhs:TaskCreationOptions, rhs:TaskCreationOptions):TaskCreationOptions;
	@:op(A ^ B) static function xor(lhs:TaskCreationOptions, rhs:TaskCreationOptions):TaskCreationOptions;
	@:op(~A) static function complement(value:TaskCreationOptions):TaskCreationOptions;
}
