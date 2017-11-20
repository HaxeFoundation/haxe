package unit.issues;

private class Foo {
	private var __a:Int;
	public var b:Int;
	public function new() {}
}

class Issue6133 extends unit.Test {
	function test() {
		var foo = new Foo();
		switch foo {
			case { b: 5 }:
		}
	}
}