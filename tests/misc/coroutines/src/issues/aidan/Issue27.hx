package issues.aidan;
import haxe.coro.context.Key;
import haxe.coro.context.Element;
import haxe.coro.Coroutine;

class DebugName extends Element<DebugName> {
	static public var key:Key<DebugName> = Key.createNew("DebugName");

	public var name:String;

	public function new(name:String) {
		super(key);
		this.name = name;
	}

	public function toString() {
		return '[DebugName: $name]';
	}
}

class Issue27 extends utest.Test {
	function test() {
		@:coroutine
		function setDebug(name:String) {
			Coroutine.suspend(cont -> {
				cont.context.set(DebugName.key, new DebugName(name));
				cont.resume(null, null);
			});
		}

		var log = [];

		@:coroutine
		function logDebug() {
			Coroutine.suspend(cont -> {
				log.push(cont.context.get(DebugName.key).name);
				cont.resume(null, null);
			});
		}

		@:coroutine
		function modifyDebug(name:String) {
			Coroutine.suspend(cont -> {
				cont.context.get(DebugName.key).name = name;
				cont.resume(null, null);
			});
		}
		@:coroutine
		function test() {
			setDebug("first name");
			logDebug();
			modifyDebug("second name");
			logDebug();
			return log.join(", ");
		}
		Assert.equals("first name, second name", Coroutine.run(test));
	}
}