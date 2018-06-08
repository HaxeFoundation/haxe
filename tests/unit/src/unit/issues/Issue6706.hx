package unit.issues;

class Issue6706 extends unit.Test {

	static inline function foo( d : Dynamic ):Dynamic {
		return d;
	}

	function test() {
		var v:Float = 12;
		HelperMacros.typedAs((null : Dynamic), foo(v));
	}
}