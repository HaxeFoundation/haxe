package cs.system.diagnostics;

/** Provides trace event data specific to a thread and a process. */
@:native("System.Diagnostics.TraceEventCache")
extern class TraceEventCache {
	/**
	 * Gets the call stack for the current thread.
	 * @return A string containing stack trace information. This value can be an empty
	 * string ("").
	 */
	var Callstack(default, never):String;
	/**
	 * Gets the date and time at which the event trace occurred.
	 * @return A  structure whose value is a date and time expressed in Coordinated
	 * Universal Time (UTC).
	 */
	var DateTime(default, never):cs.system.DateTime;
	/**
	 * Gets the correlation data, contained in a stack.
	 * @return A  containing correlation data.
	 */
	var LogicalOperationStack(default, never):cs.system.collections.Stack;
	/**
	 * Gets the unique identifier of the current process.
	 * @return The system-generated unique identifier of the current process.
	 */
	var ProcessId(default, never):Int;
	/**
	 * Gets a unique identifier for the current managed thread.
	 * @return A string that represents a unique integer identifier for this managed
	 * thread.
	 */
	var ThreadId(default, never):String;
	/**
	 * Gets the current number of ticks in the timer mechanism.
	 * @return The tick counter value of the underlying timer mechanism.
	 */
	var Timestamp(default, never):haxe.Int64;
	function new():Void;
}
