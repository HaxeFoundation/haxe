package cs.system.diagnostics;

/** Provides a set of methods and properties that you can use to accurately measure elapsed time. */
@:native("System.Diagnostics.Stopwatch")
extern class Stopwatch {
	/** Gets the frequency of the timer as the number of ticks per second. This field is read-only. */
	static var Frequency(default, never):haxe.Int64;
	/** Indicates whether the timer is based on a high-resolution performance counter. This field is read-only. */
	static var IsHighResolution(default, never):Bool;
	/**
	 * Gets the total elapsed time measured by the current instance.
	 * @return A read-only  representing the total elapsed time measured by the current
	 * instance.
	 */
	var Elapsed(default, never):cs.system.TimeSpan;
	/**
	 * Gets the total elapsed time measured by the current instance, in milliseconds.
	 * @return A read-only long integer representing the total number of milliseconds
	 * measured by the current instance.
	 */
	var ElapsedMilliseconds(default, never):haxe.Int64;
	/**
	 * Gets the total elapsed time measured by the current instance, in timer ticks.
	 * @return A read-only long integer representing the total number of timer ticks
	 * measured by the current instance.
	 */
	var ElapsedTicks(default, never):haxe.Int64;
	/**
	 * Gets a value indicating whether the  timer is running.
	 * @return if the  instance is currently running and measuring elapsed time for an
	 * interval; otherwise, .
	 */
	var IsRunning(default, never):Bool;
	function new():Void;
	/**
	 * Gets the current number of ticks in the timer mechanism.
	 * @return A long integer representing the tick counter value of the underlying
	 * timer mechanism.
	 */
	static function GetTimestamp():haxe.Int64;
	/**
	 * Initializes a new  instance, sets the elapsed time property to zero, and starts
	 * measuring elapsed time.
	 * @return A  that has just begun measuring elapsed time.
	 */
	static function StartNew():cs.system.diagnostics.Stopwatch;
	/** Stops time interval measurement and resets the elapsed time to zero. */
	function Reset():Void;
	/** Stops time interval measurement, resets the elapsed time to zero, and starts measuring elapsed time. */
	function Restart():Void;
	/** Starts, or resumes, measuring elapsed time for an interval. */
	function Start():Void;
	/** Stops measuring elapsed time for an interval. */
	function Stop():Void;
}
