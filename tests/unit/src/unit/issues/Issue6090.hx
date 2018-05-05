package unit.issues;

class Issue6090 extends Test {
	public function test() {
		eq(null, [][0]);
	}
}
