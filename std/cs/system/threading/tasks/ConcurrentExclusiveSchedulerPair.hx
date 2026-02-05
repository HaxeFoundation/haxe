package cs.system.threading.tasks;

/** Provides task schedulers that coordinate to execute tasks while ensuring that concurrent tasks may run concurrently and exclusive tasks never do. */
@:native("System.Threading.Tasks.ConcurrentExclusiveSchedulerPair")
extern class ConcurrentExclusiveSchedulerPair {
	/**
	 * Gets a  that will complete when the scheduler has completed processing.
	 * @return The asynchronous operation that will complete when the scheduler
	 * finishes processing.
	 */
	var Completion(default, never):cs.system.threading.tasks.Task;
	/**
	 * Gets a  that can be used to schedule tasks to this pair that may run
	 * concurrently with other tasks on this pair.
	 * @return An object that can be used to schedule tasks concurrently.
	 */
	var ConcurrentScheduler(default, never):cs.system.threading.tasks.TaskScheduler;
	/**
	 * Gets a  that can be used to schedule tasks to this pair that must run
	 * exclusively with regards to other tasks on this pair.
	 * @return An object that can be used to schedule tasks that do not run
	 * concurrently with other tasks.
	 */
	var ExclusiveScheduler(default, never):cs.system.threading.tasks.TaskScheduler;
	@:overload(function():Void {})
	@:overload(function(taskScheduler:cs.system.threading.tasks.TaskScheduler):Void {})
	@:overload(function(taskScheduler:cs.system.threading.tasks.TaskScheduler, maxConcurrencyLevel:Int):Void {})
	function new(taskScheduler:cs.system.threading.tasks.TaskScheduler, maxConcurrencyLevel:Int, maxItemsPerTask:Int):Void;
	/** Informs the scheduler pair that it should not accept any more tasks. */
	function Complete():Void;
}
