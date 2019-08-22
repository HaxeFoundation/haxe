package unit.issues;

class Issue4246 extends Test {
	public static function and() { }
	public static function _and() { }
	function test() {
		and();
		_and();
		noAssert();
	}
}