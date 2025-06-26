package unit.issues;

class Issue10135 extends Test {
	function doEncode() {
		var w = 0;

		var i = 0;
		while (Math.random() > 0.5) {
			while (i < 80) {
				w = 1;
			}
		}
	}

	function test() {
		utest.Assert.pass();
	}
}
