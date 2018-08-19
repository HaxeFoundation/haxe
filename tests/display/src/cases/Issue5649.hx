package cases;

class Issue5649 extends DisplayTestCase {
	/**
	class Main {
		public static function main():{-1-} {-2-} {}
	}
	**/
	function test() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
	}
}