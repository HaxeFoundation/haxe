package cs.system.threading;

/** Defines a lock that supports single writers and multiple readers. */
@:native("System.Threading.ReaderWriterLock")
extern class ReaderWriterLock extends cs.system.runtime.constrainedexecution.CriticalFinalizerObject {
	/**
	 * Gets a value indicating whether the current thread holds a reader lock.
	 * @return if the current thread holds a reader lock; otherwise, .
	 */
	var IsReaderLockHeld(default, never):Bool;
	/**
	 * Gets a value indicating whether the current thread holds the writer lock.
	 * @return if the current thread holds the writer lock; otherwise, .
	 */
	var IsWriterLockHeld(default, never):Bool;
	/**
	 * Gets the current sequence number.
	 * @return The current sequence number.
	 */
	var WriterSeqNum(default, never):Int;
	function new():Void;
	@:overload(function(millisecondsTimeout:Int):Void {})
	/**
	 * Acquires a reader lock, using an  value for the time-out.
	 * @param millisecondsTimeout The time-out in milliseconds.
	 */
	function AcquireReaderLock(timeout:cs.system.TimeSpan):Void;
	@:overload(function(millisecondsTimeout:Int):Void {})
	/**
	 * Acquires the writer lock, using an  value for the time-out.
	 * @param millisecondsTimeout The time-out in milliseconds.
	 */
	function AcquireWriterLock(timeout:cs.system.TimeSpan):Void;
	/**
	 * Indicates whether the writer lock has been granted to any thread since the
	 * sequence number was obtained.
	 * @param seqNum The sequence number.
	 * @return if the writer lock has been granted to any thread since the sequence
	 * number was obtained; otherwise, .
	 */
	function AnyWritersSince(seqNum:Int):Bool;
	/**
	 * Restores the lock status of the thread to what it was before  was called.
	 * @param lockCookie A  returned by .
	 */
	function DowngradeFromWriterLock(lockCookie:cs.Ref<cs.system.threading.LockCookie>):Void;
	/**
	 * Releases the lock, regardless of the number of times the thread acquired the
	 * lock.
	 * @return A  value representing the released lock.
	 */
	function ReleaseLock():cs.system.threading.LockCookie;
	/** Decrements the lock count. */
	function ReleaseReaderLock():Void;
	/** Decrements the lock count on the writer lock. */
	function ReleaseWriterLock():Void;
	/**
	 * Restores the lock status of the thread to what it was before calling .
	 * @param lockCookie A  returned by .
	 */
	function RestoreLock(lockCookie:cs.Ref<cs.system.threading.LockCookie>):Void;
	@:overload(function(millisecondsTimeout:Int):cs.system.threading.LockCookie {})
	/**
	 * Upgrades a reader lock to the writer lock, using an  value for the time-out.
	 * @param millisecondsTimeout The time-out in milliseconds.
	 * @return A  value.
	 */
	function UpgradeToWriterLock(timeout:cs.system.TimeSpan):cs.system.threading.LockCookie;
}
