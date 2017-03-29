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
		var v = Type.getInstanceFields(B);
		eq(2, v.length);
		t(v.indexOf("a") != -1);
		t(v.indexOf("b") != -1);
		aeq(['a'], Type.getInstanceFields(A));
	}
}
