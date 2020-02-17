package cases;

class Issue6423 extends DisplayTestCase {
	/**
		class Main {
			static function main() {}

			macro function foo(expr:haxe.macro.Expr, field:String) {
				switch (expr) {
					case macro $expr.{-1-}$fie{-2-}ld{-3-}:
						expr;
				}
			}
		}
	**/
	function test() {
		eq(range(1, 3), position(pos(2)));
		eq("String", type(pos(2)));
	}
}
