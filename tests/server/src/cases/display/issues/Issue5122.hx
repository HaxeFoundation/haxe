package cases.display.issues;

class Issue5122 extends DisplayTestCase {
	/**
		class Main {
		    static function main() {
		        inline function start() {
		            return "foo";
		        }

		        {-1-}start();
		    }
		}
	**/
	function testInlineFunctionType(_) {
		eq("() -> String", type(1));
	}
}
