package haxe.coro.schedulers;

import haxe.coro.context.Key;
import haxe.coro.context.Element;

abstract class Scheduler extends Element<Scheduler> {
	public static final key:Key<Scheduler> = Key.createNew('Scheduler');

	function new() {
		super(key);
	}

	public abstract function schedule(func:() -> Void):Void;

	public abstract function scheduleIn(func:() -> Void, ms:Int):Void;
}