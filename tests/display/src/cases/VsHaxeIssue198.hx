package cases;

class VsHaxeIssue198 extends DisplayTestCase {
	/**
	'foo.{-1-}${"foo.{-2-}".{-3-}}';
	**/
	@:funcCode function test() {
		// TODO
		// eq(true, noCompletionPoint(fields.bind(pos(1))));
		// eq(true, noCompletionPoint(fields.bind(pos(2))));
		eq(true, hasField(fields(pos(3)), "length", "Int"));
	}
}