package unit.issues;

class Issue5078 extends Test {
	static function getToLower() return "ABC".toLowerCase;

	function test() {
		#if !(js || php)
		eq(getToLower()(), "abc");
		#end
	}
}