package unit.issues;

import haxe.ds.Vector;

class Issue3579 extends Test {
	function test() {
		var a = ["a", "b"],
			b = [1, 2, 3, 4],
			c:Array<Single> = [1.1, 1.2, 1.3, 1.4],
			d = [new SomeClass(), new SomeClass()];
		var a1 = Vector.fromArrayCopy(a),
			b1 = Vector.fromArrayCopy(b),
			c1 = Vector.fromArrayCopy(c),
			d1 = Vector.fromArrayCopy(d);
		eq(a[0], a1[0]);
		eq(b[0], b1[0]);
		eq(c[0], c1[0]);
		eq(d[0], d1[0]);
		eq("a", a1[0]);
		eq(1, b1[0]);
		eq(11, Std.int(c1[0] * 10));
		t(d1[0] != null);
	}
}

private class SomeClass {
	public function new() {}
}

#if (!java && !cs)
private typedef Single = Float;
#end
