package unit.issues;

private typedef IntVec = haxe.ds.Vector<Int>;
private typedef IntVec2D = haxe.ds.Vector<IntVec>;

class Issue7016 extends unit.Test {
	function test() {
		var a = new IntVec2D(10);
		a[0] = new IntVec(10);
		a[0][0] = 1;
		a[0][1] = 2;
		t(a[0][0] == 1);
		f(a[0][1] == a[0][0]);
		t(a[0][1] == a[0][0] + 1);
	}
}
