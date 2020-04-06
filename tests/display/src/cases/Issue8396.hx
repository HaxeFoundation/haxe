package cases;

class Issue8396 extends DisplayTestCase {
	/**
		enum abstract Test(String) {
			var Foo;
			var Bar;

			function foobar() {
				{-1-}
			}
		}
	**/
	function test() {
		var r = toplevel(pos(1));
		eq(true, hasToplevel(r, "member", "Bar"));
		eq(false, hasToplevel(r, "enumabstract", "Bar"));
	}
}
