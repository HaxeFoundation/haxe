package cases;

class Issue7086 extends DisplayTestCase {
	/**
	class Main {
		public static function main() {
			~/{-1-}/;
		}
	}
	**/
	function test() {
		noCompletionPoint(toplevel.bind(pos(1)));
	}
}