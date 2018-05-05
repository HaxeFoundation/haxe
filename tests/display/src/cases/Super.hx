package cases;

class Super extends DisplayTestCase {
	/**
	class Base<T> {
		public {-1-}function new() { }{-2-}
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
	{-1-}class Base<T> {
		public {-4-}function test() { }{-5-}
	}{-2-}
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
		eq("Void -> Void", type(pos(6)));
	}
}