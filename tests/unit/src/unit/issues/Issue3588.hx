package unit.issues;

private class Foo<T> {
	public var fooField:T;

	public function new(fooField:T) {
		this.fooField = fooField;
	}
}

class Issue3588 extends Test {
	function test() {
		var foo = new Foo<Null<Float>>(null);
		eq(null, foo.fooField);
	}
}