package unit.issues;

import utest.Assert;

class Issue12447 extends Test {
	static macro function foo() {
		var arr = [1, 3, 4, 5];
		return macro [$a{[for (i in arr) macro $v{i}]}];
	}

	#if !macro
	function test() {
		utest.Assert.same([1, 3, 4, 5], foo());
	}
	#end
}
