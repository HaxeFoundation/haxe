package unit.issues;

import unit.Test;

class Issue9218 extends Test {
	public function test() {
		eq(-1, format(0));
	}

	static function format(w:Null<Int>) {
		if (whatever) {
			w--;
		}
		return w;
	}


	static var whatever = true;
}