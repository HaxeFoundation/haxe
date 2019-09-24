package unit.issues;

class Issue8845 extends unit.Test {
	static var a = 255;
	static var b = -8;

	function test() {
		eq(7, a % b);
	}
}