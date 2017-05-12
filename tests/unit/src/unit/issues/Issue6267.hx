package unit.issues;

class Issue6267 extends unit.Test {

	function test() {
		eq(doTest(0), 1);
	}

	static function doTest(v:Int) {
		while (true) {
			switch (v) {
				case 0: return call({f: impure()});
				case 1: break;
				case _: break;
			}
		}
		return 42;
	}

	static function call(o:{f:Int}) return o.f;

	@:pure(false) static function impure() return 1;
}
