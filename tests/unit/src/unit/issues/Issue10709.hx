package unit.issues;

class Issue10709 extends unit.Test {
	function test() {
		eq("null", Std.string(foo()));
	}

	static function foo():Null<Map<String, String>> {
		return null;
	}
}
