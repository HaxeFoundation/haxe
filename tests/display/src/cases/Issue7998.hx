package cases;

class Issue7998 extends DisplayTestCase {
	/**
		var subject:{
			var iterator:Int;
			var keyValueIterator:Float;
		};
		subject.{-1-}
	**/
	@:funcCode function test() {
		var fields = fields(pos(1));
		eq(true, hasField(fields, "iterator", "Int", "var"));
		eq(true, hasField(fields, "keyValueIterator", "Float", "var"));
	}
}
