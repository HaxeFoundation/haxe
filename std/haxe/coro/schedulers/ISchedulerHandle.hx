package haxe.coro.schedulers;

/**
 * Handle representing a scheduled function.
 */
interface ISchedulerHandle {
	/**
	 * Close the handle thereby cancelleting future execution of the scheduled function.
	 * If the function is currently executing or has already executed nothing happens.
	 * This function is thread safe and allowed to be called multiple times.
	 */
	function close() : Void;
}