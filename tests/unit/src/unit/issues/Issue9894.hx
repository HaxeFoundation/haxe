package unit.issues;

class Issue9894 extends unit.Test {
	function test() {
		#if (/* Hello */ debug)
		#end
		utest.Assert.pass();
	}
}
