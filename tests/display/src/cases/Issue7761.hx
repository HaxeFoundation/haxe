package cases;

class Issue7761 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				Boolean.{-1-}
			}
		}

		@:forwardStatics
		abstract Boolean(BooleanClass) {}

		extern class BooleanClass {
			static function foo():Int;
		}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "foo", "() -> Int"));
	}
}
