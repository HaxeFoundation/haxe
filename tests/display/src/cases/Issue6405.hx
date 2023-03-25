package cases;

class Issue6405 extends DisplayTestCase {
	/**
		import haxe.macro.Expr;
		import haxe.macro.Context;
		using haxe.macro.ExprTools;

		class Macros {

			public static macro function makeTypeDef( {-2-}e{-3-} : Expr ) {
				var t = Context.getType({-1-}e{-5-}.{-4-}toString());
				return macro {};
			}

		}
	**/
	function test() {
		eq(range(2, 3), position(pos(1)));
		var usage = usage(pos(2));
		arrayEq([range(1, 5)], usage);
		eq("haxe.macro.Expr", type(pos(1)));
		var fields = fields(pos(4));
		eq(true, hasField(fields, "expr", "haxe.macro.ExprDef"));
		eq(true, hasField(fields, "toString", "() -> String"));
	}
}
