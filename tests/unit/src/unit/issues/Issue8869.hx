package unit.issues;

class Issue8869 extends Test {
	function test() {
		noAssert();
	}

	function checkIntersectionConstraintInParentheses<T:(haxe.Constraints.Constructible<Void->Void> & Dummy)>(cl:Class<T>) {}
}

private class Dummy {
	public function new() {}
}