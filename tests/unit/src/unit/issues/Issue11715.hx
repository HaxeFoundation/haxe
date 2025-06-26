package unit.issues;

private enum abstract Foo(Null<Int>) {
	public var A = 0;
	public var B = 1;
}

class Issue11715 extends Test {
	var b1: Null<Int> = null;
	var b2: Foo = null;
	function test() {
		var t1 = switch (b1) {
			case 0: "A";
			case 1: "B";
			default: "C";
		};
		eq("C", t1);
		var t2 = switch (b2) {
			case A: "A";
			case B: "B";
			default: "C";
		};
		eq("C", t2);
	}
}
