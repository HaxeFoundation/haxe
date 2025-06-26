package unit.issues;

import haxe.ds.Option;

private class TestDefaultTypeParameter<T = Option<String>> {
	final data:T;

	public function new(data) {
		this.data = data;
	}
}

class Issue11161 extends Test {
	function test() {
		utest.Assert.pass();
	}
}
