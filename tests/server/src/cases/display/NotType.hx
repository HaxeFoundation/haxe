package cases.display;

class NotType extends DisplayTestCase {
	/**
		abstract {-1-}A(Int) {}
	**/
	function testAbstractDecl(_) {
		eq("A", type(1));
	}
}
