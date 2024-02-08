package unit.issues;

import utest.Assert;

class Issue8506 extends unit.Test {
	static var s = "hi";
	function test() {
		switch s {}
		Assert.pass();
	}
}