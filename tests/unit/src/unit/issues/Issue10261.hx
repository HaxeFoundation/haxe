package unit.issues;

import utest.Assert;

class Issue10261 extends Test {
	function call(x:Int) {
		return "";
	}

	function write() {
		return {
			while (true) {}
			call(0);
		}
	}

	function test() {
		Assert.pass();
	}
}
