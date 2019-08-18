package cases;

class Issue5172 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				for ({-3-}i{-1-} in 0...10) {
					{-4-}i{-2-};
				}
			}
		}
	**/
	function test() {
		eq("Int", type(pos(1)));
		eq("Int", type(pos(2)));
		eq(range(3, 1), position(pos(1)));
		arrayEq([range(4, 2)], usage(pos(1)));
	}
}
