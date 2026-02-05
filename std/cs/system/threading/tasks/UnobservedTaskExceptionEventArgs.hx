package cs.system.threading.tasks;

/** Provides data for the event that is raised when a faulted 's exception goes unobserved. */
@:native("System.Threading.Tasks.UnobservedTaskExceptionEventArgs")
extern class UnobservedTaskExceptionEventArgs extends cs.system.EventArgs {
	/**
	 * The Exception that went unobserved.
	 * @return The Exception that went unobserved.
	 */
	var Exception(default, never):cs.system.AggregateException;
	/**
	 * Gets whether this exception has been marked as "observed."
	 * @return true if this exception has been marked as "observed"; otherwise false.
	 */
	var Observed(default, never):Bool;
	function new(exception:cs.system.AggregateException):Void;
	/** Marks the  as "observed," thus preventing it from triggering exception escalation policy which, by default, terminates the process. */
	function SetObserved():Void;
}
