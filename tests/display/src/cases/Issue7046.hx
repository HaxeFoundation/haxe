package cases;

class Issue7046 extends DisplayTestCase {
	/**
		import Ar{-1-}ray;
		using Ar{-2-}ray;
	**/
	function test() {
		eq("Array<Array.T>", type(pos(1)));
		eq("Array<Array.T>", type(pos(2)));
	}
}
