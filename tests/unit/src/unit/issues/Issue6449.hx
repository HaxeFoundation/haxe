package unit.issues;

class Issue6449 extends unit.Test {
	#if js
	function test() {
		t(doTest(Math.NaN));
		f(doTest(1.5));
	}

	static function doTest(isNaN:Float):Bool {
		return js.Syntax.code("isNaN")(isNaN);
	}
	#end
}