package cs.system.componentmodel;

/** Tracks the lifetime of an asynchronous operation. */
@:native("System.ComponentModel.AsyncOperation")
extern class AsyncOperation {
	/**
	 * Gets the  object that was passed to the constructor.
	 * @return The  object that was passed to the constructor.
	 */
	var SynchronizationContext(default, never):cs.system.threading.SynchronizationContext;
	/**
	 * Gets or sets an object used to uniquely identify an asynchronous operation.
	 * @return The state object passed to the asynchronous method invocation.
	 */
	var UserSuppliedState(default, never):Dynamic;
	/** Ends the lifetime of an asynchronous operation. */
	function OperationCompleted():Void;
	/**
	 * Invokes a delegate on the thread or context appropriate for the application
	 * model.
	 * @param d A  object that wraps the delegate to be called when the operation ends.
	 * @param arg An argument for the delegate contained in the  parameter.
	 */
	function Post(d:cs.system.threading.SendOrPostCallback, arg:Dynamic):Void;
	/**
	 * Ends the lifetime of an asynchronous operation.
	 * @param d A  object that wraps the delegate to be called when the operation ends.
	 * @param arg An argument for the delegate contained in the  parameter.
	 */
	function PostOperationCompleted(d:cs.system.threading.SendOrPostCallback, arg:Dynamic):Void;
}
