package cases;

class Issue8737 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				#if macro
				te{-1-}st;
				#else
				te{-2-}st;
				#end
			}

			/** Test doc **\/
			macro static function {-4-}test{-5-}() {
				te{-3-}st;
				return macro null;
			}
		}
	**/
	function test() {
		function check(i:Int) {
			eq("() -> haxe.macro.Expr", type(pos(i)));
			eq("Test doc", doc(pos(i)));
			eq(range(4, 5), position(pos(i)));
		}

		for (i in 1...4) check(i);
	}

	/**
		class Main {
			static function main() {
				te{-1-}st();
			}

			/** Test doc **\/
			macro static function {-2-}test{-3-}() {
				return macro null;
			}
		}
	**/
	function testSimple() {
		eq("() -> haxe.macro.Expr", type(pos(1)));
		eq("Test doc", doc(pos(1)));
		eq(range(2, 3), position(pos(1)));
	}
}
