package unit.issues;

import unit.Test;

class Issue9232 extends Test {
	public function test() {
		eq(1, doubleIntDiv(9, 3));
	}

	static function doubleIntDiv(a:Int, b:Int):Int {
		return Std.int(Std.int(a / b) / b);
	}
}