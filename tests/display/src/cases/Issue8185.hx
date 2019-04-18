package cases;

class Issue8185 extends DisplayTestCase {
	function test() {
		eq(1, length());
		eq(1, name());
	}

	static function length() return 1;
	static function name() return 2;
}
