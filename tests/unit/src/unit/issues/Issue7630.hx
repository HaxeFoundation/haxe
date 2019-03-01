package unit.issues;

class Issue7630 extends unit.Test {
	#if js
	function test() {
		var r = Math.random();

    	func(inlineMe(if (r > 0.5) 42 else 21));
		noAssert();
	}

	@:pure(false)
	static function func(x:Float) { }

	static inline function inlineMe(code: Int) {
		var code2 = code;
		return js.Syntax.code("{0} + 1", code2);
	}
	#end
}