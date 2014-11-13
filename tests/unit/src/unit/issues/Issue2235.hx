package unit.issues;
import unit.Test;

private abstract A(Int) from Int {
    function f() {}
    static function fs() {}
}

class Issue2235 extends Test {
	function test() {
        var a:A = 0;
        t(unit.TestType.typeError(a.f()));
        t(unit.TestType.typeError(a.f));
		t(unit.TestType.typeError(A.fs()));
		t(unit.TestType.typeError(A.fs));
	}
}