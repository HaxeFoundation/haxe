package cases;

class Issue9044 extends DisplayTestCase {
	/**
		class Child extends Base {
			public function new() {}

			override function fu{-1-}nc() {
				super.{-2-}func{-3-}();
			}
		}

		class Base {
			public function func() {}
		}

		class Main {
			static function main() {
				var c = new Child();
				var base:Base = c;
				c.{-4-}func{-5-}();
				base.{-6-}func{-7-}();
			}
		}
	**/
	function testUsageBase() {
		arrayEq([range(2, 3), range(4, 5), range(6, 7)], usage(pos(1), true));
	}
}
