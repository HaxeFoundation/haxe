package unit.issues;

private typedef Foo9662 = {
	function make():Foo9662;
}

private class FooLike9662 {
	public function new() {}
	public function make():FooLike9662 return this;
}

class Issue9662 extends Test {
	function test() {
		var _foo:Foo9662 = new FooLike9662();
		t(_foo != null);
	}
}
