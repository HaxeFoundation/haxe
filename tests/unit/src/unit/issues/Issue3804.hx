package unit.issues;

@:generic
private class C<T> {
	public function new() {}
}

class Issue3804 extends Test {
	function test() {
		var v:C<Int> = new C();
	}
}