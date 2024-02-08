package unit.issues;
import unit.Test;

function sort<T:{next:T}>(l:T) {}

private class C {
	public var next:C;

	public function new() {}
}

class Issue9603 extends Test {
	function test() {
		sort(new C());
		utest.Assert.pass();
	}
}