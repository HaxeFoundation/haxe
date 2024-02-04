package unit.issues;

@:structInit
private class BarImpl {
	public function new() {}
}

@:structInit
private class FooImpl {
	public var x:Int;

	public function new(x:Int) {
		this.x = x;
	}
}

@:forward
private abstract Foo(FooImpl) from FooImpl to FooImpl {
	public function new(x:Int) {
		this = new FooImpl(x);
	}

	@:from
	static public function fromVec4(v:BarImpl):Foo {
		return new Foo(1);
	}
}

class Issue11535 extends Test {
	function test() {
		var v:Foo = {x: 2};
		eq(2, v.x);
	}
}
