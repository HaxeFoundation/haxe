package cs.system.threading;

/** Represents a lightweight alternative to  that limits the number of threads that can access a resource or pool of resources concurrently. */
@:native("System.Threading.SemaphoreSlim")
extern class SemaphoreSlim {
	/**
	 * Returns a  that can be used to wait on the semaphore.
	 * @return A  that can be used to wait on the semaphore.
	 */
	var AvailableWaitHandle(default, never):cs.system.threading.WaitHandle;
	/**
	 * Gets the number of remaining threads that can enter the  object.
	 * @return The number of remaining threads that can enter the semaphore.
	 */
	var CurrentCount(default, never):Int;
	@:overload(function(initialCount:Int):Void {})
	function new(initialCount:Int, maxCount:Int):Void;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	@:overload(function():Int {})
	/**
	 * Releases the  object once.
	 * @return The previous count of the .
	 */
	function Release(releaseCount:Int):Int;
	@:overload(function():Void {})
	@:overload(function(millisecondsTimeout:Int):Bool {})
	@:overload(function(cancellationToken:cs.system.threading.CancellationToken):Void {})
	@:overload(function(timeout:cs.system.TimeSpan):Bool {})
	@:overload(function(millisecondsTimeout:Int, cancellationToken:cs.system.threading.CancellationToken):Bool {})
	/** Blocks the current thread until it can enter the . */
	function Wait(timeout:cs.system.TimeSpan, cancellationToken:cs.system.threading.CancellationToken):Bool;
	@:overload(function():cs.system.threading.tasks.Task {})
	@:overload(function(millisecondsTimeout:Int):cs.system.threading.tasks.Task_1<Bool> {})
	@:overload(function(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function(timeout:cs.system.TimeSpan):cs.system.threading.tasks.Task_1<Bool> {})
	@:overload(function(millisecondsTimeout:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Bool> {})
	/**
	 * Asynchronously waits to enter the .
	 * @return A task that will complete when the semaphore has been entered.
	 */
	function WaitAsync(timeout:cs.system.TimeSpan, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Bool>;
}
