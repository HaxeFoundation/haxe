package cases;

class Type extends DisplayTestCase {
	/**
	abstract A{-1-}(Int) {}
	**/
	function testAbstractDecl() {
		eq("cases.A", type(pos(1)));
	}
}