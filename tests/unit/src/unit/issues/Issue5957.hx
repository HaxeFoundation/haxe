package unit.issues;

import haxe.ds.Vector;

class Issue5957 extends Test {
	static var foo:Vector<Vector<Int>> = {
		var vec = new Vector<Vector<Int>>(10);
		vec[0] = new Vector<Int>(2);
		vec[0][0] = 0;
		vec;
	}

	public function test() {
		foo[0][0] += 1;
		eq(1, foo[0][0]);
	}
}