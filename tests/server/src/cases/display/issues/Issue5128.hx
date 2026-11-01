package cases.display.issues;

class Issue5128 extends DisplayTestCase {
	/**
		class Main {
			static function {-1-}main():Void {}
		}
	**/
	function testVoidFunctionType(_) {
		eq("() -> Void", type(1));
	}
}
