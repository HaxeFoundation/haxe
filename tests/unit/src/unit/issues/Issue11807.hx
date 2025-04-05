package unit.issues;

class Issue11807 extends Test {
	function test() {
		var test = null;

		do {
			test = {type: 5};
		} while (test.type != 5);

		eq(5, test.type);
	}
}
