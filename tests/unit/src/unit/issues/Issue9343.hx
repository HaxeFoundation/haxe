package unit.issues;

class Issue9343 extends unit.Test {
	function test() {
		eq(1.0, getFloat());
	}

	static function getFloat():Float {
		return getUInt() / 0xFFFFFFFF;
	}

	static function getUInt():UInt {
		return 0xFFFFFFFF;
	}
}