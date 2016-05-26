package unit.issues;

class Issue4949 extends Test {
	var gems = [1,2,3];

	public function foo() {
		function onEnd(b) {
			next(b);
		}
		var f = function() {
			var ids = [for (i in 0...gems.length) if(gems[i]> 5) i]; // Main has no field push
		}
	}

	function next(d) { }

	function test() {
		foo();
	}
}