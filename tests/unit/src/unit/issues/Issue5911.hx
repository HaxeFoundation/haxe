package unit.issues;

class Issue5911 extends unit.Test {
	function test() {
		var x = 5;
		var advance = function () {
			x++;
		};
		var cur = x;
		advance();
		eq(5, cur);
	}
}