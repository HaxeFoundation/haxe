package cases;

class Issue9044 extends DisplayTestCase {
	/**
		class Child extends Base {
			public function new() {}

			override function f{-1-}unc() {
				super.{-2-}func{-3-}();
			}
		}

		class GrandChild extends Child {
			override function func() {
				super.{-5-}func{-6-}();
			}
		}

		class Base {
			public function func() {}
		}

		class Main {
			static function main() {
				var c = new Child();
				c.{-8-}func{-9-}();
				var base:Base = c;
				base.{-10-}func{-11-}();
				var g = new GrandChild();
				g.{-12-}func{-13-}();
			}
		}
	**/
	function testUsageBase() {
		arrayEq([range(2, 3), range(5, 6), range(8, 9), range(10, 11), range(12, 13)], usage(pos(1), WithBaseAndDescendants));
		arrayEq([range(5, 6), range(8, 9), range(12, 13)], usage(pos(1), WithDescendants));
	}
}
