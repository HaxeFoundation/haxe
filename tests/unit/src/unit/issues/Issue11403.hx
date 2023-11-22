package unit.issues;

class Issue11403 extends Test {
	public static macro function getValues() {
		return macro [1];
	}

	function test() {
		for (v in getValues()) {
			eq(1, v);
		}
	}
}
