package cases;

class Issue7020 extends DisplayTestCase {
	/**
		import String as {-2-}ExprAc{-4-}cess{-3-};

		class Main {
			public static function main() {
				var access:ExprA{-1-}ccess;
			}
		}
	**/
	function test() {
		// eq(range(2, 3), position(pos(1)));
		eq(range(2, 3), position(pos(4)));
		eq("String", type(pos(4)));
	}
}
