package unit.issues;

class Issue4757 extends Test {

	function test() {
		eq('ca: 2, cb: 1', cunion(1, 2));
	}

	function test2() {
		var cb = getInt();
		var result = "";
		try {
			throw 1;
		} catch (ca:Int) {
			var tmp = ca;
			ca = cb;
			cb = tmp;
			result = 'ca: $ca, cb: $cb';
		}
		eq('ca: 2, cb: 1', result);
	}

	static function getInt() return 2;

	static function cunion(ca:Int, cb:Int) {
		var tmp = ca;
		ca = cb;
		cb = tmp;
		return 'ca: $ca, cb: $cb';
	}
}