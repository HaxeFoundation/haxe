package unit.issues;

class Issue6720 extends unit.Test {
	static var a = 0;
	static var b = 1;

	function test() {
		t(a != 0 != (b != 0));
		f(a == 0 == (b == 0));
	}
}
