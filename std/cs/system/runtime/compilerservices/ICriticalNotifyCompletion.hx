package cs.system.runtime.compilerservices;

/** Represents an awaiter that schedules continuations when an await operation completes. */
@:native("System.Runtime.CompilerServices.ICriticalNotifyCompletion")
extern interface ICriticalNotifyCompletion extends cs.system.runtime.compilerservices.INotifyCompletion {
	/**
	 * Schedules the continuation action that's invoked when the instance completes.
	 * @param continuation The action to invoke when the operation completes.
	 */
	function UnsafeOnCompleted(continuation:cs.system.Action):Void;
}
