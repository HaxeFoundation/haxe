package cases;

class Issue7148 extends DisplayTestCase {
	/**
	class Main {
		static function main() {
			function {-1-}local{-2-}() {}
			lo{-3-}cal;

			inline function {-4-}local{-5-}() {}
			lo{-6-}cal;
		}
	}
	**/
	function test() {
		eq(range(1, 2), position(pos(3)));
		eq(range(4, 5), position(pos(6)));
	}
}