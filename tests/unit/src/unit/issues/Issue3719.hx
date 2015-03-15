package unit.issues;

class Issue3719 extends Test {
	function test() {
		var s:String = cast null;
		f("null" == s);
	}
}