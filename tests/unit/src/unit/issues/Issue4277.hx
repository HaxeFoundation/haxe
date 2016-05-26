package unit.issues;

class Issue4277 extends Test {
	function test() {
		t(unit.TestType.typeError(fancyInlineMethod));
	}

	@:extern private inline static function fancyInlineMethod(y:Int = 3):Int {
		var x = 5;
		return x + (x += x) - y;
	}
}