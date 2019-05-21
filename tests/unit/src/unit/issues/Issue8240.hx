package unit.issues;

private class Suite {
	public function new() {}
}

class Issue8240 extends unit.Test {
	function test() {
		var suite = new Suite();
		var f = function() (for (c in inline new haxe.iterators.StringIteratorUnicode("")) {});
		utest.Assert.pass();
	}
}