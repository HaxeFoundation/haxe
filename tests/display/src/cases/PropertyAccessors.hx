package cases;

class PropertyAccessors extends DisplayTestCase {
	/**
	class Main {
		static var test(ge{-1-}t, se{-2-}t):String;

		static public {-3-}function get_test() return "foo"{-4-};
		static public {-5-}function set_test(s:String) return s{-6-};

		static function main() { }
	}
	**/
	function test() {
		eq(range(3, 4), position(pos(1)));
		eq(range(5, 6), position(pos(2)));
		eq("Void -> String", type(pos(1)));
		eq("s : String -> String", type(pos(2)));
	}
}