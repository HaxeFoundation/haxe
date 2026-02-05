package cs.system.threading.tasks;

/** Stores options that configure the operation of methods on the  class. */
@:native("System.Threading.Tasks.ParallelOptions")
extern class ParallelOptions {
	/**
	 * Gets or sets the  associated with this  instance.
	 * @return The token that is associated with this instance.
	 */
	var CancellationToken(default, default):cs.system.threading.CancellationToken;
	/**
	 * Gets or sets the maximum number of concurrent tasks enabled by this  instance.
	 * @return An integer that represents the maximum degree of parallelism.
	 */
	var MaxDegreeOfParallelism(default, default):Int;
	/**
	 * Gets or sets the  associated with this  instance. Setting this property to null
	 * indicates that the current scheduler should be used.
	 * @return The task scheduler that is associated with this instance.
	 */
	var TaskScheduler(default, default):cs.system.threading.tasks.TaskScheduler;
	function new():Void;
}
