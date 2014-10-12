package unit.issues;

private typedef T1<T> = {
    function foo():T;
}

private typedef T2 = {
    function foo():String;
}

private typedef T3 = {
    > T1<String>,
    > T2,
    function bar():Int;
	function foo():String;
}

private class C {
	public function foo() {
		return "foo";
	}
	public function bar() {
		return 12;
	}
	public function new() { }
}

class Issue3081 extends Test {
	function test() {
		var c:T3 = new C();
		eq("foo", c.foo());
		eq(12, c.bar());
	}
}