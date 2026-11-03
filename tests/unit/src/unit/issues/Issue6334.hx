package unit.issues;

private class Foo {
	public function new() {}
}

@:forwardStatics
private abstract AFoo(Foo) from Foo {}

private enum Bar {
	EFoo(foo:AFoo);
}

class Issue6334 extends unit.Test {
	function test() {
		var expected = new Foo();
		switch (EFoo(expected)) {
			case EFoo(foo):
				t((cast foo : Foo) == expected);
		}
	}
}
