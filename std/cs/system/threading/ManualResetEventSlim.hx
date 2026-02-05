package cs.system.threading;

/** Represents a thread synchronization event that, when signaled, must be reset manually. This class is a lightweight alternative to . */
@:native("System.Threading.ManualResetEventSlim")
extern class ManualResetEventSlim {
	/**
	 * Gets whether the event is set.
	 * @return true if the event is set; otherwise, false.
	 */
	var IsSet(default, never):Bool;
	/**
	 * Gets the number of spin waits that will occur before falling back to a
	 * kernel-based wait operation.
	 * @return Returns the number of spin waits that will occur before falling back to
	 * a kernel-based wait operation.
	 */
	var SpinCount(default, never):Int;
	/**
	 * Gets the underlying  object for this .
	 * @return The underlying  event object fore this .
	 */
	var WaitHandle(default, never):cs.system.threading.WaitHandle;
	@:overload(function():Void {})
	@:overload(function(initialState:Bool):Void {})
	function new(initialState:Bool, spinCount:Int):Void;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/** Sets the state of the event to nonsignaled, which causes threads to block. */
	function Reset():Void;
	/** Sets the state of the event to signaled, which allows one or more threads waiting on the event to proceed. */
	function Set():Void;
	@:overload(function():Void {})
	@:overload(function(millisecondsTimeout:Int):Bool {})
	@:overload(function(cancellationToken:cs.system.threading.CancellationToken):Void {})
	@:overload(function(timeout:cs.system.TimeSpan):Bool {})
	@:overload(function(millisecondsTimeout:Int, cancellationToken:cs.system.threading.CancellationToken):Bool {})
	/** Blocks the current thread until the current  is set. */
	function Wait(timeout:cs.system.TimeSpan, cancellationToken:cs.system.threading.CancellationToken):Bool;
}
