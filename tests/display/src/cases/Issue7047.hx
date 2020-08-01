package cases;

class Issue7047 extends DisplayTestCase {
	/**
		class Main {
			var f:()->{-1-}

			static function main() {}
		}
	**/
	function test() {
		eq(true, hasToplevel(toplevel(pos(1)), "type", "Array"));
	}
}
