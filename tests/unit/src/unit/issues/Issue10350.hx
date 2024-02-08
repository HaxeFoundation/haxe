package unit.issues;

class Issue10350 extends Test {
	function test() {
		t(Reflect.compare(0.1, 0.2) < 0);
		t(Reflect.compare(0.2, 0.1) > 0);
	}
}
