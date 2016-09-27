package unit.issues;

class Issue4675 extends Test {
	function test() {
		var m: Map<String, Dynamic> = new Map();
		m.set("a", 1);
		eq(1, someFunction(m));
	}

	public static function someFunction(arg: Dynamic) {
		var map: Map<String, Dynamic> = cast(arg, Map<String, Dynamic>);
		return map["a"];
	}
}