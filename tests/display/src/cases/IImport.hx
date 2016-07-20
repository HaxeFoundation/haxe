package cases;

import Types;
using Lambda;

class IImport extends DisplayTestCase {
	/**
	import ha{-1-}xe.ma{-2-}cro.Exp{-3-}rTools.Expr{-4-}ArrayTools.it{-5-}er;
	**/
	function testImport1() {
		eq(true, hasPath(fields(pos(1)), "Serializer"));
		eq(true, hasPath(fields(pos(2)), "ExprTools"));
		eq(true, hasPath(fields(pos(3)), "ExprArrayTools"));
		eq(true, hasField(fields(pos(4)), "iter", "el : Array<haxe.macro.Expr> -> f : (haxe.macro.Expr -> Void) -> Void"));
		eq("el : Array<haxe.macro.Expr> -> f : (haxe.macro.Expr -> Void) -> Void", type(pos(5)));
		// TODO: test @position display. A bit annoying because it actually ends up in other files and we can't use markers.
	}

	/**
	using ha{-1-}xe.ma{-2-}cro.Exp{-3-}rTools.Expr{-4-}ArrayTools;
	**/
	function testUsing1() {
		eq(true, hasPath(fields(pos(1)), "Serializer"));
		eq(true, hasPath(fields(pos(2)), "ExprTools"));
		eq(true, hasPath(fields(pos(3)), "ExprArrayTools"));
		eq(true, hasField(fields(pos(4)), "iter", "el : Array<haxe.macro.Expr> -> f : (haxe.macro.Expr -> Void) -> Void"));
		// TODO: test @position display. A bit annoying because it actually ends up in other files and we can't use markers.
	}
}