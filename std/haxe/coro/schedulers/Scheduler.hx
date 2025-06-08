package haxe.coro.schedulers;

import haxe.coro.context.Key;
import haxe.coro.context.IElement;

abstract class Scheduler implements IElement<Scheduler> {
	public static final key = new Key<Scheduler>('Scheduler');

	function new() {}

	public abstract function schedule(ms:Int64, func:() -> Void):ISchedulerHandle;

	public abstract function scheduleObject(obj:IScheduleObject):Void;

	public abstract function now():Int64;

	public function getKey() {
		return key;
	}
}
