package cases;

class Issue6442 extends DisplayTestCase {
	/**
		extern class Foo {
			function {-1-}b{-2-}ar{-3-}():Void;
		}
	**/
	function test() {
		eq(range(1, 3), position(pos(2)));
		eq("() -> Void", type(pos(2)));
	}
}
