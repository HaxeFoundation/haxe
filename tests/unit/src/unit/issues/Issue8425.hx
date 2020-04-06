package unit.issues;

import utest.Assert;

private abstract A(Dynamic) from Dynamic {}
private abstract B(String) {}


class Issue8425 extends unit.Test {
	function test() {
		var a:A = (null : B);
		Assert.pass();
	}
}