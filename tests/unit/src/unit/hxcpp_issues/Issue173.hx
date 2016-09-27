package unit.hxcpp_issues;

private class X1 implements Y1 {
	public function new () {}
}

private interface Y1 extends Z1 {}

private interface Z1 {}

class Issue173 extends Test {
	function test() {
		var z:Z1 = new X1();
		t(z != null);
	}
}