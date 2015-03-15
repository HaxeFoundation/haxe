package unit.issues;

class Issue3861 extends unit.Test {
	function test() {
		#if analyzer
		var a;
		var b = function() return a;
		a = 2;
		eq(2, b());
		#end
	}
}