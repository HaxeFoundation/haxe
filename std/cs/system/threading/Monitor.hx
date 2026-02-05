package cs.system.threading;

/** Provides a mechanism that synchronizes access to objects. */
@:native("System.Threading.Monitor")
extern class Monitor {
	@:overload(function(obj:Dynamic):Void {})
	/**
	 * Acquires an exclusive lock on the specified object.
	 * @param obj The object on which to acquire the monitor lock.
	 */
	static function Enter(obj:Dynamic, lockTaken:cs.Ref<Bool>):Void;
	/**
	 * Releases an exclusive lock on the specified object.
	 * @param obj The object on which to release the lock.
	 */
	static function Exit(obj:Dynamic):Void;
	/**
	 * Determines whether the current thread holds the lock on the specified object.
	 * @param obj The object to test.
	 * @return if the current thread holds the lock on ; otherwise, .
	 */
	static function IsEntered(obj:Dynamic):Bool;
	/**
	 * Notifies a thread in the waiting queue of a change in the locked object's state.
	 * @param obj The object a thread is waiting for.
	 */
	static function Pulse(obj:Dynamic):Void;
	/**
	 * Notifies all waiting threads of a change in the object's state.
	 * @param obj The object that sends the pulse.
	 */
	static function PulseAll(obj:Dynamic):Void;
	@:overload(function(obj:Dynamic):Bool {})
	@:overload(function(obj:Dynamic, lockTaken:cs.Ref<Bool>):Void {})
	@:overload(function(obj:Dynamic, millisecondsTimeout:Int):Bool {})
	@:overload(function(obj:Dynamic, timeout:cs.system.TimeSpan):Bool {})
	@:overload(function(obj:Dynamic, millisecondsTimeout:Int, lockTaken:cs.Ref<Bool>):Void {})
	/**
	 * Attempts to acquire an exclusive lock on the specified object.
	 * @param obj The object on which to acquire the lock.
	 * @return if the current thread acquires the lock; otherwise, .
	 */
	static function TryEnter(obj:Dynamic, timeout:cs.system.TimeSpan, lockTaken:cs.Ref<Bool>):Void;
	@:overload(function(obj:Dynamic):Bool {})
	@:overload(function(obj:Dynamic, millisecondsTimeout:Int):Bool {})
	@:overload(function(obj:Dynamic, timeout:cs.system.TimeSpan):Bool {})
	@:overload(function(obj:Dynamic, millisecondsTimeout:Int, exitContext:Bool):Bool {})
	/**
	 * Releases the lock on an object and blocks the current thread until it reacquires
	 * the lock.
	 * @param obj The object on which to wait.
	 * @return if the call returned because the caller reacquired the lock for the
	 * specified object. This method does not return if the lock is not reacquired.
	 */
	static function Wait(obj:Dynamic, timeout:cs.system.TimeSpan, exitContext:Bool):Bool;
}
