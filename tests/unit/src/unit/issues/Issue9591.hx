package unit.issues;

private enum E {
	A(?a:Int);
}

class Issue9591 extends Test {
	function test() {
		var x = if (Math.random() > 0.5) A(0) else A();
		utest.Assert.pass();
	}
}
