package cases;

class Issue7224 extends DisplayTestCase {
	/**
	typedef Nope = Int;

	class Bar {
		function new() {}
	}
	typedef Foo = Bar;

	class Main {
		static function main() {
			new {-1-}
		}
	}
	**/
	function test() {
		var items = toplevel(pos(1));
		eq(false, hasToplevel(items, "type", "Nope"));
		eq(true, hasToplevel(items, "type", "Bar"));
		eq(true, hasToplevel(items, "type", "Foo"));
	}
}