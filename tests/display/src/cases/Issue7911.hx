package cases;

class Issue7911 extends DisplayTestCase {
	/**
		import misc.issue7911.{-1-}
	**/
	function test() {
		var fields = fields(pos(1));
		eq(1, fields.length);
		eq(true, isField(fields[0], "Test", null, "type"));
	}
}
