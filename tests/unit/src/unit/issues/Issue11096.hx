package unit.issues;

private class Macro {
	public macro static function foo(_i:Int) {
		static var i:Int;
		var r = i;
		i = _i;
		return macro $v{"" + r}
	}
}

class Issue11096 extends Test {
	function test() {
		eq("null", Macro.foo(5));
		eq("5", Macro.foo(2));
	}
}
