package cases;

class Super extends DisplayTestCase {
	/**
		class Base<T> {
			public function {-1-}new{-2-}() { }
		}
		class Main extends Base<String> {
			function new() {
				{-4-}su{-3-}per(){-5-};
			}
		}
	**/
	function testSuperCall() {
		eq(range(1, 2), position(pos(3)));
		eq("cases.Base<String>", type(pos(3)));
		arrayEq([range(4, 5)], usage(pos(3)));
	}

	/**
		class {-1-}Base{-2-}<T> {
			public function {-4-}test{-5-}() { }
		}
		class Main extends Base<String> {
			override function test() {
				su{-3-}per.te{-6-}st();
			}
		}
	**/
	function testSuperField() {
		eq(range(1, 2), position(pos(3)));
		eq("cases.Base<String>", type(pos(3)));
		eq(range(4, 5), position(pos(6)));
		eq("() -> Void", type(pos(6)));
	}
}
