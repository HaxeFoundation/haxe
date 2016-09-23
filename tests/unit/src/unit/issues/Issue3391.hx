package unit.issues;

class Issue3391 extends Test {
	function test() {
		var v = false;
		t(unit.HelperMacros.typeError(
			var a:Int = switch (v) {
				case true:
				case false:
			}
		));
	}
}