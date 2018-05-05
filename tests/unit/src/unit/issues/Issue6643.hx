package unit.issues;

private enum MyEnum {
	MyCtor(a:Int, b:Int);
}


class Issue6643 extends unit.Test {
	function test() {
		var r = MyCtor(sideEffect(), sideEffect());
		var rA = -1;
		var rB = -1;
		switch (r) {
			case MyCtor(a, b):
				rA = a;
				rB = b;
		}
		eq(0, rA);
		eq(1, rB);
	}

	static var x = 0;

	static function sideEffect() {
		return x++;
	}
}