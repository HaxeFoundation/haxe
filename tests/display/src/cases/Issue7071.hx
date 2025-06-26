package cases;

class Issue7071 extends DisplayTestCase {
	/**
		enum Foo { Bar; }

		class Main {
			public static function main() {
				var bar = Bar;
				bar == {-1-};
				if (bar == {-2-})
			}
		}
	**/
	function test() {
		eq(true, isToplevel(toplevel(pos(1))[0], "bar"));
		eq(true, isToplevel(toplevel(pos(2))[0], "bar"));
	}
}
