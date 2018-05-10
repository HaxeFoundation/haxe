package cases;

class Issue5166 extends DisplayTestCase {
	/**
		enum abstract E(Int) {
			var {-2-}A{-1-} = 5;
		}

	**/
	function test() {
		eq("cases.E", type(pos(2)));
		eq(range(2, 1), position(pos(2)));
	}
}