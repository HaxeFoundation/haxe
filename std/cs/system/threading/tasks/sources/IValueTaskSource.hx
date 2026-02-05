package cs.system.threading.tasks.sources;

/** Represents an object that can be wrapped by a . */
@:native("System.Threading.Tasks.Sources.IValueTaskSource")
extern interface IValueTaskSource {
	/**
	 * Gets the result of the .
	 * @param token An opaque value that was provided to the  constructor.
	 */
	function GetResult(token:cs.Int16):Void;
	/**
	 * Gets the status of the current operation.
	 * @param token Opaque value that was provided to the 's constructor.
	 * @return The status of the current operation.
	 */
	function GetStatus(token:cs.Int16):cs.system.threading.tasks.sources.ValueTaskSourceStatus;
	/**
	 * Schedules the continuation action for this .
	 * @param continuation The continuation to invoke when the operation has completed.
	 * @param state The state object to pass to  when it's invoked.
	 * @param token An opaque value that was provided to the 's constructor.
	 * @param flags The flags describing the behavior of the continuation.
	 */
	function OnCompleted(continuation:cs.system.Action_1<Dynamic>, state:Dynamic, token:cs.Int16, flags:cs.system.threading.tasks.sources.ValueTaskSourceOnCompletedFlags):Void;
}
