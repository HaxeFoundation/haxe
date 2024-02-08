package unit.issues;

class Issue9624 extends unit.Test {
	function test() {
		var result = 0;
		var index = 0;

		function f() {
			while (index < 5) {
				index = index + 1;
				var index = index;
				function capture() {
					result += index;
				}
				// prevent inlining everything
				capture();
				capture();
			}
		}

		f();
		// prevent inlining everything
		index = 0;
		f();

		eq(60, result);
	}
}
