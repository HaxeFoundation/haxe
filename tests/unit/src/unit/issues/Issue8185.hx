package unit.issues;

class Issue8185 extends Test {
	function test() {
		eq(1, length());
		eq(2, name());
		eq(1, TestVar.length);
		eq(2, TestVar.name);
	}

	static function length() return 1;
	static function name() return 2;
}

private class TestVar {
	static public var length = 1;
	static public var name = 2;
}