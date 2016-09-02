package unit.issues;

class Issue4810 extends Test {
	static public var DEFAULT(get, null): Bool;
	static function get_DEFAULT()
		return true;
	function test() {
		eq(true, DEFAULT);
	}
}
