package unit.issues;

enum E1 {
	A(i:Int);
	B;
}

enum E2 {
	A(s:String);
	B;
}

class Issue2623 extends unit.Test {
	public function test() {
		var v1:E1 = A(1);
		var v2 = A("foo");
		var v3:E1 = B;
		var v4 = B;

		eq(Type.getEnum(v1), E1);
		eq(Type.getEnum(v2), E2);
		eq(Type.getEnum(v3), E1);
		eq(Type.getEnum(v4), E2);
	}
}