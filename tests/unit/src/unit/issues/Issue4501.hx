package unit.issues;

private class Dummy<T> {
	public function new (arg:Null<T>) {}
}

class Issue4501 extends Test {
	function test() {
		new Dummy(null);
	}
}