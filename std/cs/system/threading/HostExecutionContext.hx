package cs.system.threading;

/** Encapsulates and propagates the host execution context across threads. */
@:native("System.Threading.HostExecutionContext")
extern class HostExecutionContext {
	/**
	 * Gets or sets the state of the host execution context.
	 * @return An object representing the host execution context state.
	 */
	var State(default, default):Dynamic;
	@:overload(function():Void {})
	function new(state:Dynamic):Void;
	/**
	 * Creates a copy of the current host execution context.
	 * @return A  object representing the host context for the current thread.
	 */
	function CreateCopy():cs.system.threading.HostExecutionContext;
	@:overload(function():Void {})
	/** Releases all resources used by the current instance of the  class. */
	function Dispose(disposing:Bool):Void;
}
