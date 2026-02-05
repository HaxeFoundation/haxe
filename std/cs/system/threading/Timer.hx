package cs.system.threading;

/** Provides a mechanism for executing a method on a thread pool thread at specified intervals. This class cannot be inherited. */
@:native("System.Threading.Timer")
extern class Timer extends cs.system.MarshalByRefObject {
	@:overload(function(callback:cs.system.threading.TimerCallback):Void {})
	@:overload(function(callback:cs.system.threading.TimerCallback, state:Dynamic, dueTime:Int, period:Int):Void {})
	@:overload(function(callback:cs.system.threading.TimerCallback, state:Dynamic, dueTime:haxe.Int64, period:haxe.Int64):Void {})
	@:overload(function(callback:cs.system.threading.TimerCallback, state:Dynamic, dueTime:cs.system.TimeSpan, period:cs.system.TimeSpan):Void {})
	function new(callback:cs.system.threading.TimerCallback, state:Dynamic, dueTime:cs.UInt, period:cs.UInt):Void;
	@:overload(function(dueTime:Int, period:Int):Bool {})
	@:overload(function(dueTime:haxe.Int64, period:haxe.Int64):Bool {})
	@:overload(function(dueTime:cs.system.TimeSpan, period:cs.system.TimeSpan):Bool {})
	/**
	 * Changes the start time and the interval between method invocations for a timer,
	 * using 32-bit signed integers to measure time intervals.
	 * @param dueTime The amount of time to delay before the invoking the callback
	 * method specified when the  was constructed, in milliseconds. Specify  to prevent
	 * the timer from restarting. Specify zero (0) to restart the timer immediately.
	 * @param period The time interval between invocations of the callback method
	 * specified when the  was constructed, in milliseconds. Specify  to disable
	 * periodic signaling.
	 * @return if the timer was successfully updated; otherwise, .
	 */
	function Change(dueTime:cs.UInt, period:cs.UInt):Bool;
	@:overload(function():Void {})
	/** Releases all resources used by the current instance of . */
	function Dispose(notifyObject:cs.system.threading.WaitHandle):Bool;
	/**
	 * Releases all resources used by the current instance of .
	 * @return A  that completes when all work associated with the timer has ceased.
	 */
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
}
