package unit.issues;

private interface I<T> {
	function foo(v:T):Void;
}

private abstract class Base implements I<Int> {
	public function new() {}
}

private class Real extends Base {
	public function foo(v:Int) {}
}

class Issue10748 extends Test {
	function test() {
		var r = new Real();
		r.foo(33);
		utest.Assert.pass();
	}
}
