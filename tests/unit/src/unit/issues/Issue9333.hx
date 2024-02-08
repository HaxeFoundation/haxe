package unit.issues;

class Issue9333 extends unit.Test {
	function test() {
		t(new EReg("b", "").matchSub("aba", 0, -1));
	}
}