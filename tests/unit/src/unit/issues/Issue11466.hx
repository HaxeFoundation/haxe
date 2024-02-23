package unit.issues;

class Issue11466 extends unit.Test {
	var b = 10;
	function test() {
		var x = 0;
		try {
			x = b;
			throw '';
		} catch(_) {
			x += 1;
		}
		eq(11, x);
	}
}
