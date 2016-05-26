package unit.issues;

@:enum
private abstract Test2(Int) to Int {
	var PROP = 123;
}

private abstract Test3(Int) {
	static public var PROP = 123;
}

class Issue4122 extends Test {
	function test() {
		eq(130, Test2.PROP + 7);
		unit.TestType.typedAs(Test2.PROP + 7, 7);
		feq(130.5, Test2.PROP + 7.5);
		unit.TestType.typedAs(Test2.PROP + 7.5, 7.5);

		eq(130, Test3.PROP + 7);
		unit.TestType.typedAs(Test3.PROP + 7, 7);
		feq(130.5, Test3.PROP + 7.5);
		unit.TestType.typedAs(Test3.PROP + 7.5, 7.5);
	}
}