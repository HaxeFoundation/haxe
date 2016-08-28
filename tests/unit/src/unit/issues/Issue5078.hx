package unit.issues;

class Issue5078 extends Test {
	#if !(js || php || lua)
	static function getToLower() return "ABC".toLowerCase;
	#end

	function test() {
		#if !(js || php || lua)
		eq(getToLower()(), "abc");
		#end
	}
}