package unit.issues;

class Issue3731 extends Test {
	function test() {
		var arr = [];
		make()(1);
		eq(1, arr.length);
		eq(1, arr[0]);
	}

	macro static public function make() {
		return macro arr.push;
	}
}