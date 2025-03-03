package unit.issues;

class Issue11998 extends unit.Test {
	static var caughtVar = {
		try {
			throw "foo";
		} catch (s:String) {
			s;
		}
	}

	public function test() {
		eq("foo", caughtVar);
	}
}
