package unit.issues;

class Issue4889 extends Test {
	function test() {
		var f = getFloat();
		t(!(f > Math.NaN));
	}

	static function getFloat() {
		return 1.0;
	}
}