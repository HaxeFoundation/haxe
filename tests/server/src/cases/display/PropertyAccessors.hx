package cases.display;

class PropertyAccessors extends DisplayTestCase {
	/**
		class Main {
			static var test(ge{-1-}t, se{-2-}t):String;

			static public function {-3-}get_test{-4-}() return "foo";
			static public function {-5-}set_test{-6-}(s:String) return s;

			static function main() { }
		}
	**/
	function test(_) {
		Assert.same(range(3, 4), position(1));
		Assert.same(range(5, 6), position(2));
		eq("() -> String", type(1));
		eq("(s : String) -> String", type(2));
	}
}
