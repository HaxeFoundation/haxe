package haxe.coro.dispatchers;

import haxe.coro.context.Key;
import haxe.coro.context.IElement;

abstract class Dispatcher implements IElement<Dispatcher> {
	public static final key = new Key<Dispatcher>("Dispatcher");

	public abstract function dispatch(obj:IScheduleObject):Void;

	public function getKey() {
		return key;
	}
}