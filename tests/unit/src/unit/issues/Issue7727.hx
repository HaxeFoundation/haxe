package unit.issues;

private enum E {
	A;
	B;
}

private interface I {
	var e:E;
}

class Issue7727 extends unit.Test implements I {
	public var e:E;

	function test() {
		e = A;
		eq(1, run(this));
	}

	static function run(i:I) {
		switch(i.e) {
			case A: return 1;
			case B: return 2;
		}
	}
}