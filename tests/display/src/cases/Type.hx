package cases;

class Type extends DisplayTestCase {
	/**
	abstract {-1-}A(Int) {}
	**/
	function testAbstractDecl() {
		eq("A", type(pos(1)));
	}
}