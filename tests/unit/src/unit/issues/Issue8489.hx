package unit.issues;

import utest.Assert;

class Issue8489 extends unit.Test {
	function test() {
		Assert.pass();
	}

	inline static function fun() {
		call(fun);
	}

	static function call(f:()->Void) f();
}