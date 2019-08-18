package unit.issues;

// order of definition matters here, see https://github.com/HaxeFoundation/haxe/issues/6016

private interface I<T> {
	function f(v:T):Int;
}

private class C implements I<E> {
	public function new() {}
	public function f(v:E) return 42;
}

private enum E {
	A;
}

class Issue6016 extends Test {
	var c:I<E>;

	function test(){
		c = new C();
		eq(c.f(A), 42);
	}
}
