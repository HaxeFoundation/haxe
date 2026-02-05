package cs.system.componentmodel;

/** Provides concurrency management for classes that support asynchronous method calls. This class cannot be inherited. */
@:native("System.ComponentModel.AsyncOperationManager")
extern class AsyncOperationManager {
	/**
	 * Gets or sets the synchronization context for the asynchronous operation.
	 * @return The synchronization context for the asynchronous operation.
	 */
	static var SynchronizationContext(default, default):cs.system.threading.SynchronizationContext;
	/**
	 * Returns an  for tracking the duration of a particular asynchronous operation.
	 * @param userSuppliedState An object used to associate a piece of client state,
	 * such as a task ID, with a particular asynchronous operation.
	 * @return An  that you can use to track the duration of an asynchronous method
	 * invocation.
	 */
	static function CreateOperation(userSuppliedState:Dynamic):cs.system.componentmodel.AsyncOperation;
}
