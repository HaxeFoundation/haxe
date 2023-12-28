package cases;

import Types;

using Lambda;

class IImport extends DisplayTestCase {
	/**
		import ha{-1-}xe.{-6-}ma{-2-}cro.{-7-}Exp{-3-}rTools.{-8-}Expr{-4-}ArrayTools.{-9-}it{-5-}er;
	**/
	function testImport1() {
		eq(true, hasPath(fields(pos(2)), "Serializer"));
		eq(true, hasPath(fields(pos(6)), "Serializer"));
		eq(true, hasPath(fields(pos(3)), "ExprTools"));
		eq(true, hasPath(fields(pos(7)), "ExprTools"));
		eq(true, hasPath(fields(pos(4)), "ExprArrayTools"));
		eq(true, hasPath(fields(pos(8)), "ExprArrayTools"));
		eq(true, hasField(fields(pos(5)), "iter", "(el : Array<haxe.macro.Expr>, f : (haxe.macro.Expr -> Void)) -> Void"));
		eq(true, hasField(fields(pos(9)), "iter", "(el : Array<haxe.macro.Expr>, f : (haxe.macro.Expr -> Void)) -> Void"));
		eq("(el : Array<haxe.macro.Expr>, f : (haxe.macro.Expr -> Void)) -> Void", type(pos(5)));
		// TODO: test @position display. A bit annoying because it actually ends up in other files and we can't use markers.
	}

	/**
		import haxe.{-1-}
	**/
	function testImport2() {
		eq(true, hasPath(fields(pos(1)), "Serializer"));
	}

	/**
		import haxe.Serializer.{-1-}
	**/
	// function testImport3() {
	// 	eq(true, hasPath(fields(pos(1)), "run"));
	// 	eq(true, hasPath(fields(pos(1)), "Serializer"));
	// }

	/**
		using ha{-1-}xe.{-5-}ma{-2-}cro.{-6-}Exp{-3-}rTools.{-7-}Expr{-4-}ArrayTools;
	**/
	function testUsing1() {
		eq(true, hasPath(fields(pos(2)), "Serializer"));
		eq(true, hasPath(fields(pos(5)), "Serializer"));
		eq(true, hasPath(fields(pos(3)), "ExprTools"));
		eq(true, hasPath(fields(pos(6)), "ExprTools"));
		eq(true, hasPath(fields(pos(4)), "ExprArrayTools"));
		eq(true, hasPath(fields(pos(7)), "ExprArrayTools"));
		// eq(true, hasField(fields(pos(8)), "iter", "el : Array<haxe.macro.Expr> -> f : (haxe.macro.Expr -> Void) -> Void"));
		// TODO: test @position display. A bit annoying because it actually ends up in other files and we can't use markers.
	}

	/**
		using haxe.{-1-}
	**/
	function testUsing2() {
		eq(true, hasPath(fields(pos(1)), "Serializer"));
	}

	/**
		using haxe.Serializer.{-1-}
	**/
	function testUsing3() {
		eq(false, hasPath(fields(pos(1)), "run"));
		eq(true, hasPath(fields(pos(1)), "Serializer"));
	}

	/**
		import haxe.macro.{-1-}
	**/
	function testIssue6408() {
		eq(true, hasPath(fields(pos(1)), "Context"));
		eq(false, hasPath(fields(pos(1)), "Context.hl"));
	}
}
