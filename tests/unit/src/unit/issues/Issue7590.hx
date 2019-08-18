package unit.issues;

class Issue7590 extends unit.Test {
	function test() {
		var array = [0, 1, 2, 3, 4, 5];
		for (k in array.copy().iterator()) {
			array.remove(k);
		}

		aeq([], array);
	}
}