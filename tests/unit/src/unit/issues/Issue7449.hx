package unit.issues;

class Issue7449 extends unit.Test {
	function test() {
		eq(220, "\xDC".charCodeAt(0));
	}
}