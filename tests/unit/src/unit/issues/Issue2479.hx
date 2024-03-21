package unit.issues;

class Issue2479 extends unit.Test {
	function test() {
		var map:Map<EFoo, Int> = [];
		var k1 = new FooParam();
		var k2 = new FooParam();
		var k3 = new FooParam();
		var k4 = new FooParam();
		map[Foo(k1)] = 1;
		map[Foo(k2)] = 2;
		map[Foo(k3)] = 3;
		map[Foo(k4)] = 4;
		eq(1, map[Foo(k1)]);
		eq(2, map[Foo(k2)]);
		eq(3, map[Foo(k3)]);
		eq(4, map[Foo(k4)]);
	}
}

private class FooParam {
	public function new() {
	}
}

private enum EFoo {
	Foo(a: FooParam);
}