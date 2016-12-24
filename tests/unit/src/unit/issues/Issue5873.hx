package unit.issues;

class Issue5873 extends unit.Test {
	function test() {
		eq(1, foo("foo", "eot"));
		eq(2, foo("LF", "x"));
		eq(2, foo("LF", "LF"));
		eq(2, foo("x", "LF"));
		eq(3, foo("LV", "V"));
		eq(3, foo("LV", "T"));
		eq(3, foo("V", "T"));
		eq(3, foo("V", "V"));
		eq(4, foo("V", "x"));
	}

	static function foo(prev, next) {
		return switch([prev,next]) {
			case [_, 'eot']           : 1;   //A
			case ['LF', _]
			   | [_, 'LF']            : 2;   //B
			case ['LV' | 'V', 'V'|'T']: 3;  //C
			case _: 4;
		}
	}
}