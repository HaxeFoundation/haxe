package unit.issues;
import unit.Test;

class Issue2763 extends Test {
	function test() {
		var i = 0;
		var acc = "";
		while (call(i++) < call2(i++)) {
			acc += ";" + i;
		}
		eq(";2;4", acc);

		var acc = "";
		var i = 0;
		do {
			acc += ";" + i;
		} while (call(i++) < call2(i++));
		eq(";0;2;4", acc);
    }

	static function call(i:Int) {
		return i;
	}

	static function call2(i:Int) {
		if (i > 4) {
			return -1;
		} else {
			return i;
		}
	}
}