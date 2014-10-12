package unit.issues;

class Issue3391 extends Test {
	function test() {
        var v = false;
        t(unit.TestType.typeError(
			var a:Int = switch (v) {
				case true:
				case false:
			}
		));
	}
}