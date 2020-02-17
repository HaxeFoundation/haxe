package unit.issues;

class Issue7476 extends unit.Test {
	function test() {
		noAssert();
	}
}

private typedef FooBar = {
	var foo:Array<Foo & Bar>;
}

private typedef Foo = {}

private typedef Bar = {}