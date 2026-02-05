package cs.system.threading;

/** Provides a mutual exclusion lock primitive where a thread trying to acquire the lock waits in a loop repeatedly checking until the lock becomes available. */
@:native("System.Threading.SpinLock")
extern class SpinLock extends cs.system.ValueType {
	/**
	 * Gets whether the lock is currently held by any thread.
	 * @return true if the lock is currently held by any thread; otherwise false.
	 */
	var IsHeld(default, never):Bool;
	/**
	 * Gets whether the lock is held by the current thread.
	 * @return true if the lock is held by the current thread; otherwise false.
	 */
	var IsHeldByCurrentThread(default, never):Bool;
	/**
	 * Gets whether thread ownership tracking is enabled for this instance.
	 * @return true if thread ownership tracking is enabled for this instance;
	 * otherwise false.
	 */
	var IsThreadOwnerTrackingEnabled(default, never):Bool;
	function new(enableThreadOwnerTracking:Bool):Void;
	/**
	 * Acquires the lock in a reliable manner, such that even if an exception occurs
	 * within the method call,  can be examined reliably to determine whether the lock
	 * was acquired.
	 * @param lockTaken True if the lock is acquired; otherwise, false.  must be
	 * initialized to false prior to calling this method.
	 */
	function Enter(lockTaken:cs.Ref<Bool>):Void;
	@:overload(function():Void {})
	/** Releases the lock. */
	function Exit(useMemoryBarrier:Bool):Void;
	@:overload(function(lockTaken:cs.Ref<Bool>):Void {})
	@:overload(function(millisecondsTimeout:Int, lockTaken:cs.Ref<Bool>):Void {})
	/**
	 * Attempts to acquire the lock in a reliable manner, such that even if an
	 * exception occurs within the method call,  can be examined reliably to determine
	 * whether the lock was acquired.
	 * @param lockTaken True if the lock is acquired; otherwise, false.  must be
	 * initialized to false prior to calling this method.
	 */
	function TryEnter(timeout:cs.system.TimeSpan, lockTaken:cs.Ref<Bool>):Void;
}
