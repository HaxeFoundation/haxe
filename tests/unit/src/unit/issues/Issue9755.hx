package unit.issues;

private abstract Issue(String) {
	public function new() {
		this = "";
	}

	@:generic public function test<T>(t:T) {
		return t;
	}
}

class Issue9755 extends unit.Test {
	function test() {
		var issue = new Issue();
		eq("foo", issue.test("foo"));
		eq(12, issue.test(12));
	}
}