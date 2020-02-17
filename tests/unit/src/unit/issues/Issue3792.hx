package unit.issues;

class Issue3792 extends unit.Test {
	function test() {
		var arr = [];
		for(x in [1, 2, 3]) {
			arr.push(function() return x);
		}

		var result = [for(fn in arr) fn()];
		aeq([1, 2, 3], result);
	}
}