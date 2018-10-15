package cases;

class Issue6422 extends DisplayTestCase {
	/**
	class Main {
		static function main() {
			pro{-1-}perty;
		}

		static var {-2-}property{-3-}(get, set):Int;
		static function get_property() return 0;
		static function set_property(i) return 0;
	}
	**/
	function test() {
		eq(range(2, 3), position(pos(1)));
	}
}
