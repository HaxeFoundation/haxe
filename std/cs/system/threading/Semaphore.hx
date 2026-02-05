package cs.system.threading;

/** Limits the number of threads that can access a resource or pool of resources concurrently. */
@:native("System.Threading.Semaphore")
extern class Semaphore extends cs.system.threading.WaitHandle {
	@:overload(function(initialCount:Int, maximumCount:Int):Void {})
	@:overload(function(initialCount:Int, maximumCount:Int, name:String):Void {})
	function new(initialCount:Int, maximumCount:Int, name:String, createdNew:cs.Ref<Bool>):Void;
	/**
	 * Opens the specified named semaphore, if it already exists.
	 * @param name The name of the system semaphore to open.
	 * @return An object that represents the named system semaphore.
	 */
	static function OpenExisting(name:String):cs.system.threading.Semaphore;
	/**
	 * Opens the specified named semaphore, if it already exists, and returns a value
	 * that indicates whether the operation succeeded.
	 * @param name The name of the system semaphore to open.
	 * @param result When this method returns, contains a  object that represents the
	 * named semaphore if the call succeeded, or  if the call failed. This parameter is
	 * treated as uninitialized.
	 * @return if the named semaphore was opened successfully; otherwise, .
	 */
	static function TryOpenExisting(name:String, result:cs.Ref<cs.system.threading.Semaphore>):Bool;
	@:overload(function():Int {})
	/**
	 * Exits the semaphore and returns the previous count.
	 * @return The count on the semaphore before the  method was called.
	 */
	function Release(releaseCount:Int):Int;
}
