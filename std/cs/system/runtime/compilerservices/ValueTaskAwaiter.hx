package cs.system.runtime.compilerservices;

/** Provides an awaiter for a . */
@:native("System.Runtime.CompilerServices.ValueTaskAwaiter")
extern class ValueTaskAwaiter extends cs.system.ValueType {
	/**
	 * Gets a value that indicates whether the  has completed.
	 * @return if the ValueTask has completed; otherwise, .
	 */
	var IsCompleted(default, never):Bool;
	/** Gets the result of the ValueTask. */
	function GetResult():Void;
	/**
	 * Schedules the continuation action for this .
	 * @param continuation The continuation action for this .
	 */
	function OnCompleted(continuation:cs.system.Action):Void;
	/**
	 * Schedules the continuation action for this ValueTask.
	 * @param continuation 
	 */
	function UnsafeOnCompleted(continuation:cs.system.Action):Void;
}
