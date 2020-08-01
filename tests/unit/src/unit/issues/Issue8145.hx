package unit.issues;

using unit.issues.Issue8145.Helper;

class Issue8145 extends unit.Test {
	function test() {
		eq("foo", "foo".foo());
	}
}

private class Helper {
	public static function foo(s:String) {
		return s;
	}
}