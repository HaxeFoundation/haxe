package unit.issues;

class Issue11466 extends unit.Test {
	var b = 10;
	function test() {
		var x = 0;
		try {
			x = b;
			throw "hi";
		}catch(_) {
			eq(b, x);
		}
	}
}
