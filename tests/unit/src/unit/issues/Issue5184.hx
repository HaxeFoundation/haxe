package unit.issues;

#if !flash

class Issue5184 extends unit.Test { }

#else

import flash.display.StageQuality;

class Issue5184 extends unit.Test {
	function test() {
		eq(0, getQ(BEST));
		eq(1, getQ(HIGH));
		//eq(2, getQ(HIGH_16X16));
		//eq(3, getQ(HIGH_16X16_LINEAR));
		//eq(4, getQ(HIGH_8X8));
		//eq(5, getQ(HIGH_8X8_LINEAR));
		eq(6, getQ(LOW));
		eq(7, getQ(MEDIUM));
	}

	function getQ(q:flash.display.StageQuality) {
		return switch (q) {
			case BEST: 0;
			case HIGH: 1;
			// case HIGH_16X16: 2;
			// case HIGH_16X16_LINEAR: 3;
			// case HIGH_8X8: 4;
			// case HIGH_8X8_LINEAR: 5;
			case LOW: 6;
			case MEDIUM: 7;
			case _: throw "unknown value";
		}
	}
}

#end