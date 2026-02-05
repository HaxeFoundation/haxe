package cs.system;

/** Provides data for the event that is raised when there is an exception that is not handled in any application domain. */
@:native("System.UnhandledExceptionEventArgs")
extern class UnhandledExceptionEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the unhandled exception object.
	 * @return The unhandled exception object.
	 */
	var ExceptionObject(default, never):Dynamic;
	/**
	 * Indicates whether the common language runtime is terminating.
	 * @return if the runtime is terminating; otherwise, .
	 */
	var IsTerminating(default, never):Bool;
	function new(exception:Dynamic, isTerminating:Bool):Void;
}
