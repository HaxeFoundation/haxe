package unit.issues;

import utest.Assert;

class Issue12047 extends Test {
	function test() {
		try {
			throwCatchWrap();
		} catch (err:Issue12047) {
			trace(err);
		} catch (e) {
			Assert.pass();
		}
	}

	static function throwCatchWrap() {
		try {
			throw "fatal";
		} catch (e) {
			throw e;
		}
	}
}
