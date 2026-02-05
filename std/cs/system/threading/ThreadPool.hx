package cs.system.threading;

/** Provides a pool of threads that can be used to execute tasks, post work items, process asynchronous I/O, wait on behalf of other threads, and process timers. */
@:native("System.Threading.ThreadPool")
extern class ThreadPool {
	@:overload(function(osHandle:cs.system.IntPtr):Bool {})
	/**
	 * Binds an operating system handle to the .
	 * @param osHandle An  that holds the handle. The handle must have been opened for
	 * overlapped I/O on the unmanaged side.
	 * @return if the handle is bound; otherwise, .
	 */
	static function BindHandle(osHandle:cs.system.runtime.interopservices.SafeHandle):Bool;
	/**
	 * Retrieves the difference between the maximum number of thread pool threads
	 * returned by the  method, and the number currently active.
	 * @param workerThreads The number of available worker threads.
	 * @param completionPortThreads The number of available asynchronous I/O threads.
	 */
	static function GetAvailableThreads(workerThreads:cs.Ref<Int>, completionPortThreads:cs.Ref<Int>):Void;
	/**
	 * Retrieves the number of requests to the thread pool that can be active
	 * concurrently. All requests above that number remain queued until thread pool
	 * threads become available.
	 * @param workerThreads The maximum number of worker threads in the thread pool.
	 * @param completionPortThreads The maximum number of asynchronous I/O threads in
	 * the thread pool.
	 */
	static function GetMaxThreads(workerThreads:cs.Ref<Int>, completionPortThreads:cs.Ref<Int>):Void;
	/**
	 * Retrieves the minimum number of threads the thread pool creates on demand, as
	 * new requests are made, before switching to an algorithm for managing thread
	 * creation and destruction.
	 * @param workerThreads When this method returns, contains the minimum number of
	 * worker threads that the thread pool creates on demand.
	 * @param completionPortThreads When this method returns, contains the minimum
	 * number of asynchronous I/O threads that the thread pool creates on demand.
	 */
	static function GetMinThreads(workerThreads:cs.Ref<Int>, completionPortThreads:cs.Ref<Int>):Void;
	@:overload(function(callBack:cs.system.threading.WaitCallback):Bool {})
	@:overload(function(callBack:cs.system.threading.WaitCallback, state:Dynamic):Bool {})
	/**
	 * Queues a method for execution. The method executes when a thread pool thread
	 * becomes available.
	 * @param callBack A  that represents the method to be executed.
	 * @return if the method is successfully queued;  is thrown if the work item could
	 * not be queued.
	 */
	static function QueueUserWorkItem<TState>(callBack:cs.system.Action_1<TState>, state:TState, preferLocal:Bool):Bool;
	@:overload(function(waitObject:cs.system.threading.WaitHandle, callBack:cs.system.threading.WaitOrTimerCallback, state:Dynamic, millisecondsTimeOutInterval:Int, executeOnlyOnce:Bool):cs.system.threading.RegisteredWaitHandle {})
	@:overload(function(waitObject:cs.system.threading.WaitHandle, callBack:cs.system.threading.WaitOrTimerCallback, state:Dynamic, millisecondsTimeOutInterval:haxe.Int64, executeOnlyOnce:Bool):cs.system.threading.RegisteredWaitHandle {})
	@:overload(function(waitObject:cs.system.threading.WaitHandle, callBack:cs.system.threading.WaitOrTimerCallback, state:Dynamic, timeout:cs.system.TimeSpan, executeOnlyOnce:Bool):cs.system.threading.RegisteredWaitHandle {})
	/**
	 * Registers a delegate to wait for a , specifying a 32-bit signed integer for the
	 * time-out in milliseconds.
	 * @param waitObject The  to register. Use a  other than .
	 * @param callBack The  delegate to call when the  parameter is signaled.
	 * @param state The object that is passed to the delegate.
	 * @param millisecondsTimeOutInterval The time-out in milliseconds. If the 
	 * parameter is 0 (zero), the function tests the object's state and returns
	 * immediately. If  is -1, the function's time-out interval never elapses.
	 * @param executeOnlyOnce to indicate that the thread will no longer wait on the 
	 * parameter after the delegate has been called;  to indicate that the timer is
	 * reset every time the wait operation completes until the wait is unregistered.
	 * @return The  that encapsulates the native handle.
	 */
	static function RegisterWaitForSingleObject(waitObject:cs.system.threading.WaitHandle, callBack:cs.system.threading.WaitOrTimerCallback, state:Dynamic, millisecondsTimeOutInterval:cs.UInt, executeOnlyOnce:Bool):cs.system.threading.RegisteredWaitHandle;
	/**
	 * Sets the number of requests to the thread pool that can be active concurrently.
	 * All requests above that number remain queued until thread pool threads become
	 * available.
	 * @param workerThreads The maximum number of worker threads in the thread pool.
	 * @param completionPortThreads The maximum number of asynchronous I/O threads in
	 * the thread pool.
	 * @return if the change is successful; otherwise, .
	 */
	static function SetMaxThreads(workerThreads:Int, completionPortThreads:Int):Bool;
	/**
	 * Sets the minimum number of threads the thread pool creates on demand, as new
	 * requests are made, before switching to an algorithm for managing thread creation
	 * and destruction.
	 * @param workerThreads The minimum number of worker threads that the thread pool
	 * creates on demand.
	 * @param completionPortThreads The minimum number of asynchronous I/O threads that
	 * the thread pool creates on demand.
	 * @return if the change is successful; otherwise, .
	 */
	static function SetMinThreads(workerThreads:Int, completionPortThreads:Int):Bool;
	/**
	 * Queues an overlapped I/O operation for execution.
	 * @param overlapped The  structure to queue.
	 * @return if the operation was successfully queued to an I/O completion port;
	 * otherwise, .
	 */
	static function UnsafeQueueNativeOverlapped(overlapped:cs.Pointer<cs.system.threading.NativeOverlapped>):Bool;
	/**
	 * Queues the specified delegate to the thread pool, but does not propagate the
	 * calling stack to the worker thread.
	 * @param callBack A  that represents the delegate to invoke when a thread in the
	 * thread pool picks up the work item.
	 * @param state The object that is passed to the delegate when serviced from the
	 * thread pool.
	 * @return if the method succeeds;  is thrown if the work item could not be queued.
	 */
	static function UnsafeQueueUserWorkItem(callBack:cs.system.threading.WaitCallback, state:Dynamic):Bool;
	@:overload(function(waitObject:cs.system.threading.WaitHandle, callBack:cs.system.threading.WaitOrTimerCallback, state:Dynamic, millisecondsTimeOutInterval:Int, executeOnlyOnce:Bool):cs.system.threading.RegisteredWaitHandle {})
	@:overload(function(waitObject:cs.system.threading.WaitHandle, callBack:cs.system.threading.WaitOrTimerCallback, state:Dynamic, millisecondsTimeOutInterval:haxe.Int64, executeOnlyOnce:Bool):cs.system.threading.RegisteredWaitHandle {})
	@:overload(function(waitObject:cs.system.threading.WaitHandle, callBack:cs.system.threading.WaitOrTimerCallback, state:Dynamic, timeout:cs.system.TimeSpan, executeOnlyOnce:Bool):cs.system.threading.RegisteredWaitHandle {})
	/**
	 * Registers a delegate to wait for a , using a 32-bit signed integer for the
	 * time-out in milliseconds. This method does not propagate the calling stack to
	 * the worker thread.
	 * @param waitObject The  to register. Use a  other than .
	 * @param callBack The delegate to call when the  parameter is signaled.
	 * @param state The object that is passed to the delegate.
	 * @param millisecondsTimeOutInterval The time-out in milliseconds. If the 
	 * parameter is 0 (zero), the function tests the object's state and returns
	 * immediately. If  is -1, the function's time-out interval never elapses.
	 * @param executeOnlyOnce to indicate that the thread will no longer wait on the 
	 * parameter after the delegate has been called;  to indicate that the timer is
	 * reset every time the wait operation completes until the wait is unregistered.
	 * @return The  object that can be used to cancel the registered wait operation.
	 */
	static function UnsafeRegisterWaitForSingleObject(waitObject:cs.system.threading.WaitHandle, callBack:cs.system.threading.WaitOrTimerCallback, state:Dynamic, millisecondsTimeOutInterval:cs.UInt, executeOnlyOnce:Bool):cs.system.threading.RegisteredWaitHandle;
}
