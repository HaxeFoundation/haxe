package unit.issues;

@:generic
private class C<T> {
	public function new() { }
	public function test<S:T>(s:S) { }
}

@:generic
private class C2<T> {
	public function new() { }
	@:generic
	public function test<S:T>(s:S) { }
}

class Issue4672 extends Test {
	function test() {
		var x = new C<String>();
		x.test("foo");

		var x = new C2<String>();
		x.test("foo");
		noAssert();
	}
}