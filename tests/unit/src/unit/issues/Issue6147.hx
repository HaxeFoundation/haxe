package unit.issues;

@:keep
private class A {
	var a:Int;
}

@:keep
private class B extends A {
	var b:Int;
}

class Issue6147 extends unit.Test {
	function test() {
		aeq(['a'], Type.getInstanceFields(A));
		aeq(['a','b'], Type.getInstanceFields(B));
		aeq(['a'], Type.getInstanceFields(A));
	}
}
