package unit.issues;
import unit.Test;

using unit.issues.Issue2625;

class Issue2625 extends Test {
	function test() {
		t(unit.TestType.typeError(
			switch(1) {
				case (_.equals(2) => true) | (_.equals(3) => false):
			}
		));

		t(unit.TestType.typeError(
			switch(1) {
				case (_.equals(2) => true), (_.equals(3) => false):
			}
		));

		var s = switch(1) {
			case _.equals(1) => true: "1";
			case _: "2";
		}
		eq("1", s);
	}

	static function equals(i1:Int, i2:Int) {
		return i1 == i2;
	}
}