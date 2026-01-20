package haxe.coro.dispatchers;

import haxe.coro.schedulers.IScheduler;
import haxe.coro.context.Key;
import haxe.coro.context.IElement;

abstract class Dispatcher implements IElement<Dispatcher> {
	public static final key = new Key<Dispatcher>("Dispatcher");

	public var scheduler (get, never) : IScheduler;

	public abstract function dispatch(obj:IDispatchObject):Void;

	public abstract function get_scheduler() : IScheduler;

	public function getKey() {
		return key;
	}
}