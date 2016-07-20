package cases;

class Issue5166 extends DisplayTestCase {
	/**
		@:enum abstract E(Int) {
			{-2-}var A{-1-} = 5;{-3-}
		}

	**/
	function test() {
		eq("cases.E", type(pos(1)));
		eq(range(2, 3), position(pos(1)));
	}
}