package cases;

class Issue7211 extends DisplayTestCase {
	/**
		var s:{f:String} = null;
		switch s.{-1-}
	**/
	@:funcCode
	function test() {
		eq(true, hasField(fields(pos(1)), "f", "String"));
	}
}
