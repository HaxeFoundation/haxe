package unit.issues;

class Issue7492 extends unit.Test {
	function test() {
		var a = [1];
		a[1] = 1;
		var v = haxe.ds.Vector.fromArrayCopy(a);
		eq(2, v.length);
	}
}