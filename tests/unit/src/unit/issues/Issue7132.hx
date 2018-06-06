package unit.issues;

class Issue7132 extends unit.Test {
	static inline var x:Float = 20;

	function test() {
		HelperMacros.typedAs(x, 0.);
	}
}