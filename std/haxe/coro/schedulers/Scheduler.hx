package haxe.coro.schedulers;

import haxe.coro.context.Key;
import haxe.coro.context.IElement;

abstract class Scheduler implements IElement<Scheduler> {
	public static final key:Key<Scheduler> = Key.createNew('Scheduler');

	function new() {}

	public abstract function schedule(ms:Int, func:() -> Void):ISchedulerHandle;

	public abstract function now():Float;

	public function getKey() {
		return key;
	}
}
