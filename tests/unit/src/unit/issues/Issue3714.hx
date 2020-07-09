package unit.issues;

private class A {
	static var a:Int;
	static function f(a:Int) {}
}

class Issue3714 extends Test {
	function test() {
		eq(unit.HelperMacros.typeErrorText(A.f), "Cannot access private field f");
		eq(unit.HelperMacros.typeErrorText(@:privateAccess A.f), null);
		eq(unit.HelperMacros.typeErrorText(@:privateAccess A.f(A.a)), null);
		eq(unit.HelperMacros.typeErrorText(@:privateAccess A.f(@:noPrivateAccess A.a)), "Cannot access private field a");
	}
}
