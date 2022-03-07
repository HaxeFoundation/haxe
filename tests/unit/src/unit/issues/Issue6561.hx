package unit.issues;

private enum Log<A> {
	NotLog(msg:String):Log<String>;
}

class Issue6561 extends unit.Test {
	function test() {
		eq("hello", apply(NotLog("hello")));
	}

	@:haxe.warning("-600")
	static function apply<A>(f:Log<A>):A {
		return switch f {
			case NotLog(msg):
				msg;
		}
	}
}
