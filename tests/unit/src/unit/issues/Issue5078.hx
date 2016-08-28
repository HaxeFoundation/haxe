package unit.issues;

class Issue5078 extends Test {
	#if !php
	static function getToLower() return "ABC".toLowerCase;
	#end

	function test() {
		#if !(js || php)
		eq(getToLower()(), "abc");
		#end
	}
}