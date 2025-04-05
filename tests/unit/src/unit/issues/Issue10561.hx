package unit.issues;

class Issue10561 extends Test {
	function test() {
		eq(null, getNull() ?.length);
	}

	function getNull():String {
		return null;
	}
}
