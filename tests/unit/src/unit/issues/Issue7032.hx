package unit.issues;

class Issue7032 extends unit.Test {
	function test() {
		eq(10, def(null));
	}

	static function def(p) {
		return def2(p);
	}

	static inline function def2(?p = 10) {
		return p;
	}
}