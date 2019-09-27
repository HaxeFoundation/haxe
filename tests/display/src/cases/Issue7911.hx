package cases;

class Issue7911 extends DisplayTestCase {
	/**
		import misc.issue7911.{-1-}
	**/
	function test() {
		arrayEq([{name: "Test", type: "", kind: "type", doc: ""}], fields(pos(1)));
	}
}
