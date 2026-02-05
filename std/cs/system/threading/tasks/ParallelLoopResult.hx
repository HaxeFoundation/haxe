package cs.system.threading.tasks;

/** Provides completion status on the execution of a  loop. */
@:native("System.Threading.Tasks.ParallelLoopResult")
extern class ParallelLoopResult extends cs.system.ValueType {
	/**
	 * Gets whether the loop ran to completion, such that all iterations of the loop
	 * were executed and the loop didn't receive a request to end prematurely.
	 * @return true if the loop ran to completion; otherwise false;
	 */
	var IsCompleted(default, never):Bool;
	/**
	 * Gets the index of the lowest iteration from which  was called.
	 * @return Returns an integer that represents the lowest iteration from which the
	 * Break statement was called.
	 */
	var LowestBreakIteration(default, never):Null<haxe.Int64>;
}
