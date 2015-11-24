package unit.issues;

private interface I {}

private class C implements I {
	public function new() { }
}

class Issue4603 extends Test {
	function test() {
		var i:I = new C();
		t(Std.is(i, I));
		eq(i, cast(i, I));
	}
}