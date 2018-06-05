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
	}
}