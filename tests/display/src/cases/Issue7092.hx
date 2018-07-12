package cases;

class Issue7092 extends DisplayTestCase {
	/**
	class Main {
		public static function main() {
			new {-1-}
		}
	}

	private class PrivateClass {
		public function new() {}
	}
	**/
	function test() {
		eq(true, hasToplevel(toplevel(pos(1)), "type", "PrivateClass"));
	}
}