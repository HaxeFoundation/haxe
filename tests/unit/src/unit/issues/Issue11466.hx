package unit.issues;

private function doThrow() {
	throw "from doThrow";
}

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

	function test2() {
		var x = 0;
		try {
			x = b;
			doThrow();
		} catch(_) {
			x += 1;
		}
		eq(11, x);
	}
}
