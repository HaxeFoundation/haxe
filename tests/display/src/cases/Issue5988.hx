package cases;

class Issue5988 extends DisplayTestCase {
	/**
		class Main {
			static function f():Int;

			static function main() {
				{-1-}
			}
		}
	**/
	function test() {
		eq(true, hasToplevel(toplevel(pos(1)), "type", "Array"));
	}
}
