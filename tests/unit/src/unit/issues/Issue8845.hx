package unit.issues;

class Issue8845 extends unit.Test {
	static var a:Int;
	static var b:Int;

	// This is actually unspecified: https://haxe.org/manual/expression-operators-binops.html
	// but happens to work on all targets
	function test() {
		a = 255;
		b = -8;
		eq(7, a % b);

		a = -100;
		b = -9;
		eq(-1, a % b);
	}
}