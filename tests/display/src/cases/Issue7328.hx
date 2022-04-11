package cases;

class Issue7328 extends DisplayTestCase {
	/**
		import haxe.macro.Expr.ExprDef;

		class Main {
			public static function main() {
				switch (null:ExprDef) {
					case EConst(CIdent({-1-}s)):
					case _:
				}
			}
		}
	**/
	function test() {
		sigEq(0, [["s:String"]], signature(pos(1)));
	}
}
