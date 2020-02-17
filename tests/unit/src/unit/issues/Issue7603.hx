package unit.issues;

private abstract Foo(String) {
	@:from static public function from(bar: Bar):Foo {
		return cast bar.getName();
	}
}

private enum Bar {
	A(v: Int);
	B;
}

class Issue7603 extends unit.Test {
	function test() {
		var bar:Foo = A(200);
		noAssert();
	}
}