package cases;

class Issue7471 extends DisplayTestCase {
	/**
		return "foo" == null ? "foo".{-1-}
	**/
	@:funcCode
	function test() {
		eq(true, hasField(fields(pos(1)), "length", "Int"));
	}
}
