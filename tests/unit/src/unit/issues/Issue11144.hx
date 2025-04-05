package unit.issues;

class Issue11144 extends Test {
	function test() {
		t(false ?? false || true); // false
		t((false ?? false) || true); // true
	}
}
