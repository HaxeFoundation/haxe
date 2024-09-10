package cases;

class Issue6417 extends DisplayTestCase {
	/**
		class Main {
			static function main() {}

			macro function foo({-1-}body{-2-}:haxe.macro.Expr) {
				macro function() $bo{-3-}dy;
			}
		}
	**/
	function test() {
		eq(range(1, 2), position(pos(3)));
		eq("haxe.macro.Expr", type(pos(3)));
	}
}
