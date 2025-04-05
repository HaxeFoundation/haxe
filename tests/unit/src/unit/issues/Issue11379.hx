package unit.issues;

private class SafeNavThing {
	static public function doSafeNavThings(test:SafeNavThing) {
		test?.int = 0;
		test?.int += 1;
		test?.int++;
		++test?.int;
	}

	public var int:Int;

	public function new() {}
}

class Issue11379 extends Test {
	function test() {
		final test = new SafeNavThing();
		SafeNavThing.doSafeNavThings(test);
		eq(3, test.int);

		SafeNavThing.doSafeNavThings(null);
	}
}
