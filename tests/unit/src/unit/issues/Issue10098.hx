package unit.issues;

private enum abstract Foo(String) {
	var Bar;
}

class Issue10098 extends Test {
	function test() {
		var m = new Map<Foo,Int>();
		f(m.exists(Bar));
		m[Bar] = 123;
		t(m.exists(Bar));
	}
}
