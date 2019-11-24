package unit.issues;

class Issue5073 extends Test {
	function test() {
		function dontRunMe() {
			var done = false;
			while (!done) {
				trace(Math.random() - 1);
			}
		}
		pretendToRun(dontRunMe);
		noAssert();
	}

	static function pretendToRun(_) { }
}