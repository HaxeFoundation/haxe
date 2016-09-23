package unit.issues;

@:enum
private abstract A(Int) to Int {
	var a = 1;
	var b = 2;

	static var c = 3;
}

class Issue2939 extends Test {
	function test() {
		eq(1, a);
		eq(2, b);
		t(unit.HelperMacros.typeError(c));
	}
}