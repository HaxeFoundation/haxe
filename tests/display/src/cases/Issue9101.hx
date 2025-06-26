package cases;

class Issue9101 extends DisplayTestCase {
	/**
		typedef T = {
		?{-1-}t{-2-}e{-3-}st:Int
		}
	**/
	function testCatch_noTypeHint() {
		eq("Null<Int>", type(pos(1)));
		eq("Null<Int>", type(pos(2)));
		eq("Null<Int>", type(pos(3)));
	}
}
