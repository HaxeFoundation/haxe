package haxe.coro.schedulers;

import haxe.coro.context.Key;
import haxe.coro.context.IElement;

/**
	This is the abstract base class of all schedulers, which are used to manage the asynchronous
	behavior of coroutines. Refer to the `hxcoro.schedulers` package in the `hxcoro` haxelib for
	concrete implementations.
**/
abstract class Scheduler implements IElement<Scheduler> {
	/**
		The key which is internally used to look up schedulers.
	**/
	public static final key = new Key<Scheduler>('Scheduler');

	function new() {}

	/**
		Schedules `func` to be run `ms` milliseconds from now. Returns an `ISchedulerHandle` which
		allows cancellation.
	**/
	public abstract function schedule(ms:Int64, func:() -> Void):ISchedulerHandle;

	/**
		Schedules `obj` to run. Schedulers ensure that the order of execution follows
		first-in-first-out.
	**/
	public abstract function scheduleObject(obj:IScheduleObject):Void;

	/**
		Returns the current time in millseconds.
	**/
	public abstract function now():Int64;

	/**
		@see `IElement.getKey`
	**/
	public function getKey() {
		return key;
	}
}
