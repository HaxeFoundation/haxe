package cases.display;

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
	function testSuperCall(_) {
		Assert.same(range(1, 2), position(3));
		eq("Base<String>", type(3));
		arrayEq([range(4, 5)], usage(2));
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
	function testSuperField(_) {
		Assert.same(range(1, 2), position(3));
		eq("Base<String>", type(3));
		Assert.same(range(4, 5), position(6));
		eq("() -> Void", type(6));
	}
}
