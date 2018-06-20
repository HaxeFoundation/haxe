package unit.issues;

private enum E {
	A;
	B;
	C(v:Int);
}

class Issue7209 extends unit.Test {
	function test() {
		var allEnums = Type.allEnums(E);
		aeq(allEnums, [A,B]);

		allEnums.remove(B);
		aeq(allEnums, [A]);

		aeq(Type.allEnums(E), [A,B]);
	}
}
