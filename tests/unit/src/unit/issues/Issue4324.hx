package unit.issues;

class Issue4324 extends Test {

	function test() {
		eq(1, getInt());
	}

	static inline function getInt() {
		switch (getTrue()) {
			case true: return 1;
			case false: return 2;
		}
	}

	static function getTrue() {
		return true;
	}

}