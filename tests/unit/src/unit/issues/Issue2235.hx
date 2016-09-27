package unit.issues;
import unit.Test;

private abstract A(Int) from Int {
	function f() {}
	static function fs() {}
}

class Issue2235 extends Test {
	function test() {
		var a:A = 0;
		t(unit.HelperMacros.typeError(a.f()));
		t(unit.HelperMacros.typeError(a.f));
		t(unit.HelperMacros.typeError(A.fs()));
		t(unit.HelperMacros.typeError(A.fs));
	}
}