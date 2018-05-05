package unit.issues;

class Issue4997 extends Test {
	function test() {
		var b = true;
		if (b) {
			return;
		}

		var a = [];
		a[0] = true;
	}
}