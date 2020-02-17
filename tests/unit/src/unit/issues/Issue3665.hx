package unit.issues;

private enum abstract E(Int) {
	var A = 1;
	var B = 2;
}

class Issue3665 extends Test {
	function test() {
		var x = A;
		var x = switch (x) {
			case A: 1;
			case B: 2;
		}
		eq(1, x);
	}
}