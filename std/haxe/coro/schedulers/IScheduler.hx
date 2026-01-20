package haxe.coro.schedulers;

/**
	This is the interface of all schedulers, which are used to manage the asynchronous
	behavior of coroutines. Refer to the `hxcoro.schedulers` package in the `hxcoro` haxelib for
	concrete implementations.
**/
interface IScheduler {
	/**
		Schedules `func` to be run `ms` milliseconds from now. Returns an `ISchedulerHandle` which
		allows cancellation.
	**/
	function schedule(ms:Int64, func:() -> Void):ISchedulerHandle;

	/**
		Returns the current time in millseconds.
	**/
	function now():Int64;
}
