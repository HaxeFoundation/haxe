package unit.issues;

class Issue5274 extends unit.Test {
	function test() {
		eq("null", doMatch(null));
		eq("foobar", doMatch("foo,bar"));
	}

	static function doMatch(s:String) {
		return switch (s) {
			case null: "null";
			case _.split(",") => [s1, s2]: s1 + s2;
			default: "default";
		}
	}
}