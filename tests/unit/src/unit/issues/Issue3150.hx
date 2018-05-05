package unit.issues;

class Issue3150 extends Test {
	function test() {
		t(unit.HelperMacros.typeError(
			var d = switch (2) {
				case 3: 90;
				case var f if (f == 0 | 2): -135;
			}
		));
	}
}