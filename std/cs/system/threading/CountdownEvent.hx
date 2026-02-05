package cs.system.threading;

/** Represents a synchronization primitive that is signaled when its count reaches zero. */
@:native("System.Threading.CountdownEvent")
extern class CountdownEvent {
	/**
	 * Gets the number of remaining signals required to set the event.
	 * @return The number of remaining signals required to set the event.
	 */
	var CurrentCount(default, never):Int;
	/**
	 * Gets the numbers of signals initially required to set the event.
	 * @return The number of signals initially required to set the event.
	 */
	var InitialCount(default, never):Int;
	/**
	 * Indicates whether the  object's current count has reached zero.
	 * @return if the current count is zero; otherwise, .
	 */
	var IsSet(default, never):Bool;
	/**
	 * Gets a  that is used to wait for the event to be set.
	 * @return A  that is used to wait for the event to be set.
	 */
	var WaitHandle(default, never):cs.system.threading.WaitHandle;
	function new(initialCount:Int):Void;
	@:overload(function():Void {})
	/** Increments the 's current count by one. */
	function AddCount(signalCount:Int):Void;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	@:overload(function():Void {})
	/** Resets the  to the value of . */
	function Reset(count:Int):Void;
	@:overload(function():Bool {})
	/**
	 * Registers a signal with the , decrementing the value of .
	 * @return true if the signal caused the count to reach zero and the event was set;
	 * otherwise, false.
	 */
	function Signal(signalCount:Int):Bool;
	@:overload(function():Bool {})
	/**
	 * Attempts to increment  by one.
	 * @return true if the increment succeeded; otherwise, false. If  is already at
	 * zero, this method will return false.
	 */
	function TryAddCount(signalCount:Int):Bool;
	@:overload(function():Void {})
	@:overload(function(millisecondsTimeout:Int):Bool {})
	@:overload(function(cancellationToken:cs.system.threading.CancellationToken):Void {})
	@:overload(function(timeout:cs.system.TimeSpan):Bool {})
	@:overload(function(millisecondsTimeout:Int, cancellationToken:cs.system.threading.CancellationToken):Bool {})
	/** Blocks the current thread until the  is set. */
	function Wait(timeout:cs.system.TimeSpan, cancellationToken:cs.system.threading.CancellationToken):Bool;
}
