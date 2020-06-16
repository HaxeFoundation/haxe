package unit.issues;
import unit.Test;

class Issue9594 extends Test {
	function test() {
		var arr:Array<Array<Int>> = [];
		arr.sort(sort);
		utest.Assert.pass();
	}

	static function sort(a, b) {
		if (a.length != b.length) return a.length - b.length;
		return 0;
	}
}
