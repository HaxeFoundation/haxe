package unit.issues;

private class C {
	public var value:String;
	public function new() {
		value = "foo";
	}
}

private abstract A(C) to C {
	public function f() {
		return "bar";
	}
}

@:generic
private class G<T:{function new():Void;}> {
	public function new() {}
	public function make():T return new T();
}

class Issue4364 extends Test {
	function test() {
		var g = new G<A>();
		var a = g.make();
		eq("bar", a.f());
		eq("foo", (a : C).value);
	}
}