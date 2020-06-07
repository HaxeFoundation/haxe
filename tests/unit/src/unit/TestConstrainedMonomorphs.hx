package unit;

import utest.Assert;

class TestConstrainedMonomorphs extends Test {

	function infer(arg) {
		var s1 = arg.toUpperCase();
		var s:String = arg;
		HelperMacros.typedAs(arg, "foo");
		return s + s1;
	}

	function testNarrowingInference() {
		eq("fooFOO", infer("foo"));
	}
}