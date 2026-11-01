package unit.issues;

class Issue5019 extends unit.Test {
	static inline var VALUE:UInt = 0x9747b28c;
	static var value2:UInt = 0x9747b28c;

	function test() {
		t(VALUE > 0);
		t(value2 > 0);
		t(value2 == 0x9747b28c);
		t(VALUE == value2);
		eq(2538058380.0, (value2 : Float));
	}
}
