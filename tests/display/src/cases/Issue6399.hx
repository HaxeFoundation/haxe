package cases;

class Issue6399 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {}

			macro function foo({-1-}name{-2-}:String, {-3-}struct{-4-}:haxe.macro.Expr, {-5-}defaults{-6-}:haxe.macro.Expr) {
				return macro {
					if ($str{-7-}uct.$n{-8-}ame == null) $str{-9-}uct.$n{-10-}ame = $defa{-11-}ults.$n{-12-}ame;
				}
			}
		}
	**/
	function test() {
		for (i in [8, 10, 12]) {
			eq(range(1, 2), position(pos(i)));
			eq("String", type(pos(i)));
		}

		for (i in [7, 9]) {
			eq(range(3, 4), position(pos(i)));
			eq("haxe.macro.Expr", type(pos(i)));
		}

		eq(range(5, 6), position(pos(11)));
		eq("haxe.macro.Expr", type(pos(11)));
	}
}
