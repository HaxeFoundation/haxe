package haxe.coro.schedulers;

/**
	This is the interface of all schedulers, which are used to manage time-related behavior of
	coroutines. Refer to the `hxcoro.schedulers` package in the `hxcoro` haxelib for
	concrete implementations.
**/
interface IScheduler {
	/**
		Schedules `func` to be run `ms` milliseconds from now. Returns an `ISchedulerHandle` which
		allows cancellation.

		The exact way `func` is called is implemented-dependent. It is strongly recommended to
		return from the callback as soon as possible, e.g. by dispatching further operations
		to a dispatcher instead of executing them directly. In particular, continuations should
		not be resumed synchronously from the callback code.
	**/
	function schedule<T>(ms:Int64, cont:IContinuation<T>):ISchedulerHandle;

	/**
		Returns the current time in millseconds. The nature of this value is implementation-depdendent
		and might not necessarily relate to real time. Assumptions about its absolute value should
		not be made, only the difference between two time values has a meaning.
	**/
	function now():Int64;
}
