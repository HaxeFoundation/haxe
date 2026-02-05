package cs.system.threading;

/** Provides the basic functionality for propagating a synchronization context in various synchronization models. */
@:native("System.Threading.SynchronizationContext")
extern class SynchronizationContext {
	/**
	 * Gets the synchronization context for the current thread.
	 * @return A  object representing the current synchronization context.
	 */
	static var Current(default, never):cs.system.threading.SynchronizationContext;
	function new():Void;
	/**
	 * Sets the current synchronization context.
	 * @param syncContext The  object to be set.
	 */
	static function SetSynchronizationContext(syncContext:cs.system.threading.SynchronizationContext):Void;
	/**
	 * When overridden in a derived class, creates a copy of the synchronization
	 * context.
	 * @return A new  object.
	 */
	function CreateCopy():cs.system.threading.SynchronizationContext;
	/**
	 * Determines if wait notification is required.
	 * @return if wait notification is required; otherwise, .
	 */
	function IsWaitNotificationRequired():Bool;
	/** When overridden in a derived class, responds to the notification that an operation has completed. */
	function OperationCompleted():Void;
	/** When overridden in a derived class, responds to the notification that an operation has started. */
	function OperationStarted():Void;
	/**
	 * When overridden in a derived class, dispatches an asynchronous message to a
	 * synchronization context.
	 * @param d The  delegate to call.
	 * @param state The object passed to the delegate.
	 */
	function Post(d:cs.system.threading.SendOrPostCallback, state:Dynamic):Void;
	/**
	 * When overridden in a derived class, dispatches a synchronous message to a
	 * synchronization context.
	 * @param d The  delegate to call.
	 * @param state The object passed to the delegate.
	 */
	function Send(d:cs.system.threading.SendOrPostCallback, state:Dynamic):Void;
	/**
	 * Waits for any or all the elements in the specified array to receive a signal.
	 * @param waitHandles An array of type  that contains the native operating system
	 * handles.
	 * @param waitAll to wait for all handles;  to wait for any handle.
	 * @param millisecondsTimeout The number of milliseconds to wait, or  (-1) to wait
	 * indefinitely.
	 * @return The array index of the object that satisfied the wait.
	 */
	function Wait(waitHandles:cs.NativeArray<cs.system.IntPtr>, waitAll:Bool, millisecondsTimeout:Int):Int;
}
