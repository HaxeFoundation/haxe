package cs.system.threading.tasks;

/** Represents an object that handles the low-level work of queuing tasks onto threads. */
@:native("System.Threading.Tasks.TaskScheduler")
extern class TaskScheduler {
	/**
	 * Gets the  associated with the currently executing task.
	 * @return Returns the  associated with the currently executing task.
	 */
	static var Current(default, never):cs.system.threading.tasks.TaskScheduler;
	/**
	 * Gets the default  instance that is provided by the .NET Framework.
	 * @return Returns the default  instance.
	 */
	static var Default(default, never):cs.system.threading.tasks.TaskScheduler;
	/**
	 * Gets the unique ID for this .
	 * @return Returns the unique ID for this .
	 */
	var Id(default, never):Int;
	/**
	 * Indicates the maximum concurrency level this  is able to support.
	 * @return Returns an integer that represents the maximum concurrency level. The
	 * default scheduler returns .
	 */
	var MaximumConcurrencyLevel(default, never):Int;
	/**
	 * Creates a  associated with the current .
	 * @return A  associated with the current , as determined by .
	 */
	static function FromCurrentSynchronizationContext():cs.system.threading.tasks.TaskScheduler;
}
