package unit.issues;

class Issue5218 extends unit.Test {
	function test() {
		var a = [["a"],["b"]];
		var x = a.shift();
		a[0].push(x[0]);
		eq(2, a[0].length);
		eq("b", a[0][0]);
		eq("a", a[0][1]);
	}
}