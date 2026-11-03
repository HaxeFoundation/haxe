package unit.issues;

private class C {
	public var x:Int = 1;

	public function new() {}

	public function foo()
		return x + 10;
}

private interface I {
	var y:Int;
	function bar():Int;
}

private class D implements I {
	public var y:Int = 100;

	public function new() {}

	public function bar()
		return y + 1000;
}

private class Holder<T:C, U:I> {
	var t:T;
	var u:U;

	public function new(t:T, u:U) {
		this.t = t;
		this.u = u;
	}

	public function get()
		return t.x + t.foo() + u.y + u.bar();
}

private interface Chain {
	function doIt():Chain;
}

private class ChainImpl implements Chain {
	public var count = 0;

	public function new() {}

	public function doIt():ChainImpl {
		count++;
		return this;
	}
}

class Issue5072 extends unit.Test {
	function test() {
		eq(1212, new Holder(new C(), new D()).get());

		var c = new ChainImpl();
		chain(c);
		eq(2, c.count);
	}

	static function chain<M:Chain>(m:M) {
		m.doIt().doIt();
	}
}
