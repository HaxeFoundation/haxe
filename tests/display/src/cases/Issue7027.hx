package cases;

class Issue7027 extends DisplayTestCase {
	/**
	import haxe.macro.Expr.ExprDef.{-1-}
	}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "EBreak", "haxe.macro.ExprDef"));
	}
}
