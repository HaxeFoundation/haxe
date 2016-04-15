package unit.issues;

class Issue5078 extends Test {
	static function getToLower() return "ABC".toLowerCase;
	static function getSubstr() return "012345".substr;

	function test() {
		eq(getToLower()(), "abc");
		eq(getSubstr()(1,3), "123");
	}
}