package unit.issues;

class Issue8702 extends unit.Test {
	public function test() {
		var foo = new Foo<String>();
		var bar = new Bar<String>();
		noAssert();
	}
}

private class Foo<T> {
	inline public function new (someBool = true) {}
}

private class Bar<T> {
	inline public function new (?someBool:Bool) {}
}