package unit.issues;

class Issue8849 extends unit.Test {
	static var a:Int = -1;

	function test() {
		eq(-1, a | -1);
		eq(-1, a & -1);
	}
}