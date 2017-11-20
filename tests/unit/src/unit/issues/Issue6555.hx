package unit.issues;

class Issue6555 extends unit.Test {
	function test() {
		var v:Dynamic = inlineFunction(false);
		eq(0, v);
	}

	static inline function inlineFunction(z:Bool):Dynamic {
		if (z) {
			return [];
		} else return 0;
	}
}