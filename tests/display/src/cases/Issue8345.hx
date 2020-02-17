package cases;

class Issue8345 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var i:Test2 = null;
				i.{-1-};
			}
		}

		interface Test {
			var foo:Int;
		}

		interface Test2 extends Test {}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "foo", "Int"));
	}
}
