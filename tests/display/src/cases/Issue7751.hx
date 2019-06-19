package cases;

class Issue7751 extends DisplayTestCase {
	/**
		extern class Foo {
			function new():V{-1-}oid;
		}
	**/
	function test() {
		eq("Void", type(pos(1)));
	}
}
