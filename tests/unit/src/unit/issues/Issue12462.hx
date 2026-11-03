package unit.issues;

import utest.Assert;

class Issue12462 extends Test {
	function test() {
		inline for (i in 0...3) {
			for (j in 0...Std.random(3)) {}
		}
		Assert.pass();
	}
}
