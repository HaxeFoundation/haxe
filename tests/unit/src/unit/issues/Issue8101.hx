package unit.issues;

import utest.Assert;

class Issue8101 extends unit.Test {
	function test() {
		var a;
		f1 = () -> a = "";
		f2 = () -> a = "";
		Assert.pass();
	}

	static dynamic function f1():Void {}
	static dynamic function f2() {}
}