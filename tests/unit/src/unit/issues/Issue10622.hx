package unit.issues;

class Issue10622 extends unit.Test {
	public function test() {
		eq("h", hi() ?.charAt(0));
	}

	static function hi()
		return "hi";
}
