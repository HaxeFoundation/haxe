package unit.issues;

class Issue8264 extends Test {
	function test() {
		var b = new BClass();
		b.gen.test("a");
		eq("a", b.aclass.gen.get());
		b.genFn().test("b");
		eq("b", b.aclass.gen.get());
	}
}

private abstract ArrayTest(Array<String>) {
	public function new()
		this = [];

	public inline function test(str:String) {
		this = [str];
	}

	public function get()
		return this.join(",");
}

private class AClass {
	public var gen = new ArrayTest();

	public function new() {}
}

private class BClass {
	public var aclass = new AClass();

	public function new() {}

	public var gen(get, never):ArrayTest;

	inline function get_gen()
		return aclass.gen;

	public inline function genFn()
		return aclass.gen;
}
