package cases;

class PropertyAccessors extends DisplayTestCase {
	/**
		class Main {
			static var test(ge{-1-}t, se{-2-}t):String;

			static public function {-3-}get_test{-4-}() return "foo";
			static public function {-5-}set_test{-6-}(s:String) return s;

			static function main() { }
		}
	**/
	function test() {
		eq(range(3, 4), position(pos(1)));
		eq(range(5, 6), position(pos(2)));
		eq("() -> String", type(pos(1)));
		eq("(s : String) -> String", type(pos(2)));
	}
}
