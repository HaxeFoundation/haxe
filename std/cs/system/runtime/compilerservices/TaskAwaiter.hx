package cs.system.runtime.compilerservices;

/** Provides an object that waits for the completion of an asynchronous task. */
@:native("System.Runtime.CompilerServices.TaskAwaiter")
extern class TaskAwaiter extends cs.system.ValueType {
	/**
	 * Gets a value that indicates whether the asynchronous task has completed.
	 * @return if the task has completed; otherwise, .
	 */
	var IsCompleted(default, never):Bool;
	/** Ends the wait for the completion of the asynchronous task. */
	function GetResult():Void;
	/**
	 * Sets the action to perform when the  object stops waiting for the asynchronous
	 * task to complete.
	 * @param continuation The action to perform when the wait operation completes.
	 */
	function OnCompleted(continuation:cs.system.Action):Void;
	/**
	 * Schedules the continuation action for the asynchronous task that is associated
	 * with this awaiter.
	 * @param continuation The action to invoke when the await operation completes.
	 */
	function UnsafeOnCompleted(continuation:cs.system.Action):Void;
}
