package haxe.coro.schedulers;

import haxe.coro.context.Key;
import haxe.coro.context.IElement;

abstract class Scheduler implements IElement<Scheduler> {
	public static final key:Key<Scheduler> = Key.createNew('Scheduler');

	function new() {}

	public abstract function schedule(func:() -> Void):Void;

	public abstract function scheduleIn(func:() -> Void, ms:Int):Void;

	public function getKey() {
		return key;
	}
}
