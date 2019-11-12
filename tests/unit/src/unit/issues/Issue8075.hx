package unit.issues;

class Issue8075 extends unit.Test {
	function test() {
		var expect = #if static 0 #else null #end;
		var a = [];
		a.push(1);
		a.pop();
		a[2] = 2;
		eq(expect, a[0]);
	}
}