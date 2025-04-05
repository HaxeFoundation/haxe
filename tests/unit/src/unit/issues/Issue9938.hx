package unit.issues;

class Issue9938 extends unit.Test {
	function test() {
		var arr = [0, 1, 2];
		switch [arr.shift(), arr.shift(), arr.shift()] {
			case [a, b, c]:
				eq(0, a);
				eq(1, b);
				eq(2, c);
		}
	}
}
