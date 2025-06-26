package unit.issues;

class Issue9413 extends unit.Test {
	function test() {
		var size = 100;

		try {
			noop();
		} catch (e) {
			for (i in 0...size) {
				noop();
			}
		}
		noAssert();
	}

	@:pure(false) static function noop() {}
}