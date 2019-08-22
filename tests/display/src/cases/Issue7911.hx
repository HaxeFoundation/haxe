package cases;

class Issue7911 extends DisplayTestCase {
	/**
		import misc.issue7911.{-1-}
	**/
	function test() {
		arrayEq([{name: "Test", type: "", kind: "type"}], fields(pos(1)));
	}
}
