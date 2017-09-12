package cases;

class Issue6442 extends DisplayTestCase {
	/**
	extern class Foo {
		{-1-}function b{-2-}ar():Void;{-3-}
	}
	**/
	function test() {
		eq(range(1, 3), position(pos(2)));
		eq("Void -> Void", type(pos(2)));
	}
}
