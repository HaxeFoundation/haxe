package unit.issues;

class Issue3950 extends Test {
	function test() {
		var s = cast ("foo" : String);
		eq("foo", s);
	}
}