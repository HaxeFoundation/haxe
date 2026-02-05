package cs.system.threading;

/** Manages the execution context for the current thread. This class cannot be inherited. */
@:native("System.Threading.ExecutionContext")
extern class ExecutionContext {
	/**
	 * Captures the execution context from the current thread.
	 * @return An  object representing the execution context for the current thread.
	 */
	static function Capture():cs.system.threading.ExecutionContext;
	/**
	 * Indicates whether the flow of the execution context is currently suppressed.
	 * @return if the flow is suppressed; otherwise, .
	 */
	static function IsFlowSuppressed():Bool;
	/** Restores the flow of the execution context across asynchronous threads. */
	static function RestoreFlow():Void;
	/**
	 * Runs a method in a specified execution context on the current thread.
	 * @param executionContext The  to set.
	 * @param callback A  delegate that represents the method to be run in the provided
	 * execution context.
	 * @param state The object to pass to the callback method.
	 */
	static function Run(executionContext:cs.system.threading.ExecutionContext, callback:cs.system.threading.ContextCallback, state:Dynamic):Void;
	/**
	 * Suppresses the flow of the execution context across asynchronous threads.
	 * @return An  structure for restoring the flow.
	 */
	static function SuppressFlow():cs.system.threading.AsyncFlowControl;
	/**
	 * Creates a copy of the current execution context.
	 * @return An  object representing the current execution context.
	 */
	function CreateCopy():cs.system.threading.ExecutionContext;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/**
	 * Sets the specified  object with the logical context information needed to
	 * recreate an instance of the current execution context.
	 * @param info The  object to be populated with serialization information.
	 * @param context The  structure representing the destination context of the
	 * serialization.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
