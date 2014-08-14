package unit.issues;

private abstract E(Int) from Int to Int {
	@:arrayAccess public function access(key:Int):E {
		return this + key;
	}

	@:op(A++) function oneup():E {
		return this + 1;
	}
}

class Issue3247 extends Test {
	function test() {
		var e:E = 1;
		eq(1, e);
		eq(2, e[1]);
		eq(3, e[2]);
		eq(4, e[2]++);
		eq(5, (e[3])++);
	}
}