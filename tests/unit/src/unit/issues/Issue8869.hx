package unit.issues;

class Issue8869 extends Test {
	function test() {
		noAssert();
	}

	function checkIntersectionConstraintInParentheses<T:(haxe.Constraints.Constructible<() -> Void> & Dummy)>(cl:Class<T>) {}

	function checkFunctionTypeConstraint<T:(Int) -> Void>() {}
}

private class Dummy {
	public function new() {}
}
