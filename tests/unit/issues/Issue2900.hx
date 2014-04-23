package unit.issues;

class Issue2900 extends Test {
	function test() {
		var objs:Iterable<Int> = [10, 20];
		var fns = [for (obj in objs) function() return obj];
		eq(10, fns[0]());
		eq(20, fns[1]());
	}
}