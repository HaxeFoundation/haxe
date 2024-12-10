package cases;

using Lambda;

class Issue7059 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				{-1-}
			}
		}
	**/
	function test() {
		eq(true, hasToplevel(toplevel(pos(1)), "literal", "trace"));
	}
}
