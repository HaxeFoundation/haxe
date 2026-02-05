package cs.system.threading;

/** Represents a lock that is used to manage access to a resource, allowing multiple threads for reading or exclusive access for writing. */
@:native("System.Threading.ReaderWriterLockSlim")
extern class ReaderWriterLockSlim {
	/**
	 * Gets the total number of unique threads that have entered the lock in read mode.
	 * @return The number of unique threads that have entered the lock in read mode.
	 */
	var CurrentReadCount(default, never):Int;
	/**
	 * Gets a value that indicates whether the current thread has entered the lock in
	 * read mode.
	 * @return if the current thread has entered read mode; otherwise, .
	 */
	var IsReadLockHeld(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current thread has entered the lock in
	 * upgradeable mode.
	 * @return if the current thread has entered upgradeable mode; otherwise, .
	 */
	var IsUpgradeableReadLockHeld(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current thread has entered the lock in
	 * write mode.
	 * @return if the current thread has entered write mode; otherwise, .
	 */
	var IsWriteLockHeld(default, never):Bool;
	/**
	 * Gets a value that indicates the recursion policy for the current  object.
	 * @return One of the enumeration values that specifies the lock recursion policy.
	 */
	var RecursionPolicy(default, never):cs.system.threading.LockRecursionPolicy;
	/**
	 * Gets the number of times the current thread has entered the lock in read mode,
	 * as an indication of recursion.
	 * @return 0 (zero) if the current thread has not entered read mode, 1 if the
	 * thread has entered read mode but has not entered it recursively, or n if the
	 * thread has entered the lock recursively n - 1 times.
	 */
	var RecursiveReadCount(default, never):Int;
	/**
	 * Gets the number of times the current thread has entered the lock in upgradeable
	 * mode, as an indication of recursion.
	 * @return 0 if the current thread has not entered upgradeable mode, 1 if the
	 * thread has entered upgradeable mode but has not entered it recursively, or n if
	 * the thread has entered upgradeable mode recursively n - 1 times.
	 */
	var RecursiveUpgradeCount(default, never):Int;
	/**
	 * Gets the number of times the current thread has entered the lock in write mode,
	 * as an indication of recursion.
	 * @return 0 if the current thread has not entered write mode, 1 if the thread has
	 * entered write mode but has not entered it recursively, or n if the thread has
	 * entered write mode recursively n - 1 times.
	 */
	var RecursiveWriteCount(default, never):Int;
	/**
	 * Gets the total number of threads that are waiting to enter the lock in read
	 * mode.
	 * @return The total number of threads that are waiting to enter read mode.
	 */
	var WaitingReadCount(default, never):Int;
	/**
	 * Gets the total number of threads that are waiting to enter the lock in
	 * upgradeable mode.
	 * @return The total number of threads that are waiting to enter upgradeable mode.
	 */
	var WaitingUpgradeCount(default, never):Int;
	/**
	 * Gets the total number of threads that are waiting to enter the lock in write
	 * mode.
	 * @return The total number of threads that are waiting to enter write mode.
	 */
	var WaitingWriteCount(default, never):Int;
	@:overload(function():Void {})
	function new(recursionPolicy:cs.system.threading.LockRecursionPolicy):Void;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/** Tries to enter the lock in read mode. */
	function EnterReadLock():Void;
	/** Tries to enter the lock in upgradeable mode. */
	function EnterUpgradeableReadLock():Void;
	/** Tries to enter the lock in write mode. */
	function EnterWriteLock():Void;
	/** Reduces the recursion count for read mode, and exits read mode if the resulting count is 0 (zero). */
	function ExitReadLock():Void;
	/** Reduces the recursion count for upgradeable mode, and exits upgradeable mode if the resulting count is 0 (zero). */
	function ExitUpgradeableReadLock():Void;
	/** Reduces the recursion count for write mode, and exits write mode if the resulting count is 0 (zero). */
	function ExitWriteLock():Void;
	@:overload(function(millisecondsTimeout:Int):Bool {})
	/**
	 * Tries to enter the lock in read mode, with an optional integer time-out.
	 * @param millisecondsTimeout The number of milliseconds to wait, or -1 () to wait
	 * indefinitely.
	 * @return if the calling thread entered read mode, otherwise, .
	 */
	function TryEnterReadLock(timeout:cs.system.TimeSpan):Bool;
	@:overload(function(millisecondsTimeout:Int):Bool {})
	/**
	 * Tries to enter the lock in upgradeable mode, with an optional time-out.
	 * @param millisecondsTimeout The number of milliseconds to wait, or -1 () to wait
	 * indefinitely.
	 * @return if the calling thread entered upgradeable mode, otherwise, .
	 */
	function TryEnterUpgradeableReadLock(timeout:cs.system.TimeSpan):Bool;
	@:overload(function(millisecondsTimeout:Int):Bool {})
	/**
	 * Tries to enter the lock in write mode, with an optional time-out.
	 * @param millisecondsTimeout The number of milliseconds to wait, or -1 () to wait
	 * indefinitely.
	 * @return if the calling thread entered write mode, otherwise, .
	 */
	function TryEnterWriteLock(timeout:cs.system.TimeSpan):Bool;
}
