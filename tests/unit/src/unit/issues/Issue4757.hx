package unit.issues;

class Issue4757 extends Test {

	function test() {
		eq('ca: 2, cb: 1', cunion(1, 2));
	}

	static function cunion(ca:Int, cb:Int) {
		var tmp = ca;
		ca = cb;
		cb = tmp;
		return 'ca: $ca, cb: $cb';
	}
}