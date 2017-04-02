package unit.issues;

class Issue6067 extends unit.Test {
	function doIt(v) {
		var r = 1;
		(function() {
			if (v) {
				function f() return 1;
				r = 2;
			}
			r = 3;
		})();
		return r;
	}

	function test() {
		eq(3, doIt(true));
	}
}