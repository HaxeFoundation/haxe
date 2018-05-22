package cases;

class Issue7050 extends DisplayTestCase {
	/**
	class Main {
		static function main() {
			// {-1-}
		}
	}
	**/
	function test() {
		eq(true, noCompletionPoint(toplevel.bind(pos(1))));
	}
}