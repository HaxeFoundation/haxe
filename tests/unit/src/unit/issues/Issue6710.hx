package unit.issues;

private enum E {
	A(a:Array<String>);
}

class Issue6710 extends unit.Test {
	function test() {

		var a = [];
		var b = [];
		t(Type.enumEq(A(a), A(a)));
		f(Type.enumEq(A(a), A(b)));

		var a = [];
		var b = [];

		eq(checkEq(a, b), false);

		eq(checkEq(2000000, 2000000), true);
		eq(checkEq("basicstring", "basicstring"), true);

	}

	static function checkEq<T>(t1:T, t2:T) {
		return t1 == t2;
	}
}