package unit.issues;

class Issue6106 extends unit.Test {
	function test() {
		var code = 0;
		function assertEquals<T>(expected:T, actual:T) {
			if(expected != actual) code++;
		}

		var f = function() assertEquals(1, 1);
		f();
		noAssert();
	}
}