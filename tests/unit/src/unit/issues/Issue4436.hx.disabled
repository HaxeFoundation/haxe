package unit.issues;

import haxe.Int64;

class Issue4436 extends Test {
	function test() {
		nullableValue = 29;
		t(checkNull(29));
	}

	var nullableValue:Null<Int64>;

	private function checkNull(value:Int64) {
		if (nullableValue == value) {
			return true;
		}
		return false;
	}
}