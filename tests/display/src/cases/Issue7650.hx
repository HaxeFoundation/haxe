package cases;

class Issue7650 extends DisplayTestCase {
	/**
		new MyClasss("".{-1-}
	**/
	@:funcCode
	function test1() {
		eq(true, hasField(fields(pos(1)), "length", "Int"));
	}

	/**
		new MyClasss<MyClassss>("".{-1-}
	**/
	@:funcCode
	function test2() {
		eq(true, hasField(fields(pos(1)), "length", "Int"));
	}
}
