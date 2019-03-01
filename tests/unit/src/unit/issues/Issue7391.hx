package unit.issues;

private typedef Foo = {
	final i:Int;
}

private abstract Bar(Foo) from Foo {}

class Issue7391 extends unit.Test {
	function test() {
		var b:Bar = {i: 0};
		noAssert();
	}
}