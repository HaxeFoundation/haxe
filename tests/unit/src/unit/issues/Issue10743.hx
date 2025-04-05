package unit.issues;

@:keep
private class C {
	var o = () -> {
		return 28;
	};
}

class Issue10743 extends Test {
	function test() {
		utest.Assert.pass();
	}
}
