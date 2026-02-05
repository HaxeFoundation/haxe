package cs.system.runtime.exceptionservices;

/** Provides data for the notification event that is raised when a managed exception first occurs, before the common language runtime begins searching for event handlers. */
@:native("System.Runtime.ExceptionServices.FirstChanceExceptionEventArgs")
extern class FirstChanceExceptionEventArgs extends cs.system.EventArgs {
	/**
	 * The managed exception object that corresponds to the exception thrown in managed
	 * code.
	 * @return The newly thrown exception.
	 */
	var Exception(default, never):cs.system.Exception;
	function new(exception:cs.system.Exception):Void;
}
