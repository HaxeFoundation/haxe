package haxe.coro.dispatchers;

import haxe.coro.schedulers.Scheduler;
import haxe.coro.context.Key;
import haxe.coro.context.IElement;

abstract class Dispatcher implements IElement<Dispatcher> {
	public static final key = new Key<Dispatcher>("Dispatcher");

	public var scheduler (get, never) : Scheduler;

	public abstract function dispatch(obj:IScheduleObject):Void;

	public abstract function get_scheduler() : Scheduler;

	public function getKey() {
		return key;
	}
}