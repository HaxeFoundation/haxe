package cases;

class Issue6396 extends DisplayTestCase {
	/**
	class Main {
		public static function main() {}

		macro function foo() {
			var {-1-}name{-2-} = "name";
			return macro {
				$na{-3-}me;
			}
		}
	}
	**/
	function test() {
		eq(range(1, 2), position(pos(3)));
		eq("String", type(pos(3)));
	}
}
