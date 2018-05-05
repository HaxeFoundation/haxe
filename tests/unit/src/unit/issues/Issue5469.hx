package unit.issues;

class Issue5469 extends unit.Test {
	function test() {
		var foo : Dynamic = new Foo();
		foo.bar( StringTools.urlDecode('') );
		t(true);
	}
}

@:keep
private class Foo {
	public function new() {}
	public function bar(p: String) {}
}
