package cases;

class Issue6434 extends DisplayTestCase {
	/**
	import misc.ModuleWithPrivateType;

	class Main {
		static function main() {
			{-1-}
		}
	}
	**/
	function test() {
		var toplevel = toplevel(pos(1));
		eq(true, hasToplevel(toplevel, "type", "PublicClass"));
		// This doesn't really test what I want to test, but I don't know how
		// to test what I want to test...
		eq(false, hasToplevel(toplevel, "type", "PrivateClass"));
	}
}