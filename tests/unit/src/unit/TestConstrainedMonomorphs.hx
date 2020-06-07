package unit;

import utest.Assert;

private class MyNotString {
	var s:String;

	public function new(s:String) {
		this.s = s;
	}

	public function toUpperCase() {
		return new MyNotString(s.toUpperCase());
	}

	public function getString() {
		return s;
	}
}

class TestConstrainedMonomorphs extends Test {

	function infer(arg) {
		var s1:MyNotString = arg.toUpperCase();
		var s:MyNotString = arg;
		HelperMacros.typedAs(arg, (null : MyNotString));
		return s.getString() + s1.getString();
	}

	function testNarrowingInference() {
		eq("fooFOO", infer(new MyNotString("foo")));
	}
}