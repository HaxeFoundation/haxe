package unit.issues;

class Issue8845 extends unit.Test {
	static var a = 255;
	static var b = -8;

	function test() {
		// This is actually unspecified: https://haxe.org/manual/expression-operators-binops.html
		// but happens to work on all targets
		eq(7, a % b);
	}
}