package cases;

class Issue7577 extends DisplayTestCase {
	/**

		@:forward abstract X(String) {}

		class Main {
			static function test(x:X) {
				x.{-1-}
			}
		}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "charCodeAt", "(index : Int) -> Null<Int>"));
	}
}
