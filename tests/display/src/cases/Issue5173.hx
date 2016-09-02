package cases;

class Issue5173 extends DisplayTestCase {
	/**
	class Main {
		static function main() {
			var map = new haxe.DynamicAccess();
			for ({-3-}ke{-1-}y{-4-} in map.keys()) {
				{-5-}ke{-2-}y{-6-};
			}
		}
	}
	**/
	function test() {
		eq("String", type(pos(1)));
		eq("String", type(pos(2)));
		eq(range(3, 4), position(pos(1)));
		eq(range(3, 4), position(pos(2)));
		arrayEq([range(5, 6)], usage(pos(1)));
		arrayEq([range(5, 6)], usage(pos(2)));
	}
}