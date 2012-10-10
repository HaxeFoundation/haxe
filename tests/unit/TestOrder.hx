package unit;

using unit.MyUsing1;
using unit.MyUsing2;

enum E1 {
	C1;
}

enum E2 {
	C1;
}

class TestOrder extends Test {
	public function testUsing() {
		// Latest using should be used
		eq("foo".usingTest(), "3");
		
		// Latest enum should be used
		var c:E2 = E2.C1;
		TestType.typedAs(C1, c);
		eq(c, C1);
	}
}