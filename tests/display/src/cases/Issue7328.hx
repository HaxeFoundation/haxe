package cases;

class Issue7328 extends DisplayTestCase {
	/**
		import haxe.macro.Expr.ExprDef;

		class Main {
			public static function main() {
				switch (null:ExprDef) {
					case EConst(CInt({-1-}v)):
					case _:
				}
			}
		}
	**/
	function test() {
		sigEq(0, [["v:String"]], signature(pos(1)));
	}
}
