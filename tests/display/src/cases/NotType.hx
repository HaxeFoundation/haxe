package cases;

class NotType extends DisplayTestCase {
	/**
		abstract {-1-}A(Int) {}
	**/
	function testAbstractDecl() {
		eq("A", type(pos(1)));
	}
}
