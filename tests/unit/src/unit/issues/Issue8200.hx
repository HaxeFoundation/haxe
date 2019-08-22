package unit.issues;

class Issue8200 extends unit.Test {
	function test() {
		var a:Abstract = new Impl();
		t(a.foo.bind()());
	}
}

class Impl implements Interface {
	public function new() {}
	public function foo():Bool return true;
}

@:forward
abstract Abstract(Interface) from Interface to Interface {}

interface Interface {
	function foo():Bool;
}
