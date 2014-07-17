package unit.issues;

class Issue3150 extends Test {
	function test() {
		t(unit.TestType.typeError(
			var d = switch (2) {
				case 3: 90;
				case f if (f == 0 | 2): -135;
			}
		));
	}
}