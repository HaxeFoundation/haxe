package cs.system.runtime.compilerservices;

/** Represents an operation that schedules continuations when it completes. */
@:native("System.Runtime.CompilerServices.INotifyCompletion")
extern interface INotifyCompletion {
	/**
	 * Schedules the continuation action that's invoked when the instance completes.
	 * @param continuation The action to invoke when the operation completes.
	 */
	function OnCompleted(continuation:cs.system.Action):Void;
}
