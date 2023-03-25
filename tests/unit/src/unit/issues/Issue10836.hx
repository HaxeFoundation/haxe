package unit.issues;

class Issue10836 extends Test {
	function test() {
		var foo = new Foo();
		foo.bar = () -> 'Foo::new bar';
		eq('Foo::bar', foo.run());
	}
}

private class Foo {
	public var run:Void->String;

	public function new() {
		run = bar;
	}

	public dynamic function bar() {
		return 'Foo::bar';
	}
}