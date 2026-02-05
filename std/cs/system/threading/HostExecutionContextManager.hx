package cs.system.threading;

/** Provides the functionality that allows a common language runtime host to participate in the flow, or migration, of the execution context. */
@:native("System.Threading.HostExecutionContextManager")
extern class HostExecutionContextManager {
	function new():Void;
	/**
	 * Captures the host execution context from the current thread.
	 * @return A  object representing the host execution context of the current thread.
	 */
	function Capture():cs.system.threading.HostExecutionContext;
	/**
	 * Restores the host execution context to its prior state.
	 * @param previousState The previous context state to revert to.
	 */
	function Revert(previousState:Dynamic):Void;
	/**
	 * Sets the current host execution context to the specified host execution context.
	 * @param hostExecutionContext The  to be set.
	 * @return An object for restoring the  to its previous state.
	 */
	function SetHostExecutionContext(hostExecutionContext:cs.system.threading.HostExecutionContext):Dynamic;
}
