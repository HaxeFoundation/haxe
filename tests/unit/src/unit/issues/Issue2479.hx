package unit.issues;

import Type;

class Issue2479 extends unit.Test {
	function testAlex() {
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

	function testLptr() {
		var map:Map<ValueType, String> = new Map();
		map.set(TInt, "TInt");
		map.set(TClass(String), "TClass(String)");
		map.set(TClass(Color), "TClass(Color)");
		map.set(TClass(Length), "TClass(Length)");
		map.set(TFloat, "TFloat");

		eq("TInt", map.get(TInt));
		eq("TFloat", map.get(TFloat));
		eq("TClass(String)", map.get(TClass(String)));
		eq("TClass(Color)", map.get(TClass(Color)));
		eq("TClass(Length)", map.get(TClass(Length)));
	}
}

private class FooParam {
	public function new() {}
}

private enum EFoo {
	Foo(a:FooParam);
}

private class Color {
	public function clone() {
		return this;
	}
}

private class Length {
	public function clone() {
		return this;
	}
}
