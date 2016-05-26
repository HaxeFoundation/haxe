package unit.issues;

import unit.Test;

/**
 * ...
 * @author
 */
class Issue4743 extends Test{
	static var i(default, set):Int = 0;
	static function set_i(value) return i = value;

	static var j:Int = 0;

	public function test() {
		i = j = 1;
		eq(1, i);
		eq(1, j);
	}
}