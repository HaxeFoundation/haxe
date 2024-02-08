package unit.issues;

class Issue5078 extends Test {
	#if !(js || lua)
	static function getToLower() return "ABC".toLowerCase;
	#end

	function test() {
		#if !(js || lua)
		eq(getToLower()(), "abc");
		#else
		noAssert();
		#end
	}
}