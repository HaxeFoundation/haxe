package haxe.coro.dispatchers;

import haxe.coro.schedulers.IScheduler;
import haxe.coro.context.Key;
import haxe.coro.context.IElement;

/**
	A dispatcher manages the distribution of executable code to executors,
	allowing the asynchronous behavior of coroutines. Refer to the
	`hxcoro.dispatchers` package in the `hxcoro` haxelib for conrete implementation.
**/
abstract class Dispatcher implements IElement<Dispatcher> {
	public static final key = new Key<Dispatcher>("Dispatcher");

	/**
		The scheduler associated with `this` dispatcher. In the general case, scheduling
		functions are unaware of dispatchers and it is the responsibility of the user to
		ensure correct code distribution. See `haxe.coro.schedulers.IScheduler.schedule`
		for details.
	**/
	public var scheduler (get, never) : IScheduler;

	/**
		Dispatches `obj` to be executed. While the code execution is guaranteed (as long
		as the program doesn't deadlock), no assumptions should be made about how and when
		exactly that occurs.
	**/
	public abstract function dispatch(obj:IDispatchObject):Void;

	abstract function get_scheduler() : IScheduler;

	/**
		@see `IElement.getKey`
	**/
	public function getKey() {
		return key;
	}
}