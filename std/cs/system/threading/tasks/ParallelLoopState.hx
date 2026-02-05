package cs.system.threading.tasks;

/** Enables iterations of parallel loops to interact with other iterations. An instance of this class is provided by the  class to each loop; you can not create instances in your code. */
@:native("System.Threading.Tasks.ParallelLoopState")
extern class ParallelLoopState {
	/**
	 * Gets whether any iteration of the loop has thrown an exception that went
	 * unhandled by that iteration.
	 * @return if an unhandled exception was thrown; otherwise, .
	 */
	var IsExceptional(default, never):Bool;
	/**
	 * Gets whether any iteration of the loop has called the  method.
	 * @return if any iteration has stopped the loop by calling the  method; otherwise,
	 * .
	 */
	var IsStopped(default, never):Bool;
	/**
	 * Gets the lowest iteration of the loop from which  was called.
	 * @return The lowest iteration from which  was called. In the case of a  loop, the
	 * value is based on an internally-generated index.
	 */
	var LowestBreakIteration(default, never):Null<haxe.Int64>;
	/**
	 * Gets whether the current iteration of the loop should exit based on requests
	 * made by this or other iterations.
	 * @return if the current iteration should exit; otherwise, .
	 */
	var ShouldExitCurrentIteration(default, never):Bool;
	/** Communicates that the  loop should cease execution of iterations beyond the current iteration at the system's earliest convenience. */
	function Break():Void;
	/** Communicates that the  loop should cease execution at the system's earliest convenience. */
	function Stop():Void;
}
