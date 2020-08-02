package unit.issues;

import utest.Assert;

private abstract class C {
	abstract function f():String;

	public function test() {
		return f();
	}
}

class Issue9795 extends unit.Test {
	function test() {
		Assert.pass();
	}
}