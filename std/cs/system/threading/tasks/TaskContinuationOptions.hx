package cs.system.threading.tasks;

/** Specifies the behavior for a task that is created by using the  or  method. */
@:native("System.Threading.Tasks.TaskContinuationOptions")
extern enum abstract TaskContinuationOptions(Int) {
	var AttachedToParent = 4;
	var DenyChildAttach = 8;
	var ExecuteSynchronously = 524288;
	var HideScheduler = 16;
	var LazyCancellation = 32;
	var LongRunning = 2;
	var None = 0;
	var NotOnCanceled = 262144;
	var NotOnFaulted = 131072;
	var NotOnRanToCompletion = 65536;
	var OnlyOnCanceled = 196608;
	var OnlyOnFaulted = 327680;
	var OnlyOnRanToCompletion = 393216;
	var PreferFairness = 1;
	var RunContinuationsAsynchronously = 64;
	@:op(A | B) static function or(lhs:TaskContinuationOptions, rhs:TaskContinuationOptions):TaskContinuationOptions;
	@:op(A & B) static function and(lhs:TaskContinuationOptions, rhs:TaskContinuationOptions):TaskContinuationOptions;
	@:op(A ^ B) static function xor(lhs:TaskContinuationOptions, rhs:TaskContinuationOptions):TaskContinuationOptions;
	@:op(~A) static function complement(value:TaskContinuationOptions):TaskContinuationOptions;
}
