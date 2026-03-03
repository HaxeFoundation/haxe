package cases.display;

class IImport extends DisplayTestCase {
	/**
		import ha{-1-}xe.{-6-}ma{-2-}cro.{-7-}Exp{-3-}rTools.{-8-}Expr{-4-}ArrayTools.{-9-}it{-5-}er;
	**/
	function testImport1(_) {
		eq(true, hasPath(fields(2), "Serializer"));
		eq(true, hasPath(fields(6), "Serializer"));
		eq(true, hasPath(fields(3), "ExprTools"));
		eq(true, hasPath(fields(7), "ExprTools"));
		eq(true, hasPath(fields(4), "ExprArrayTools"));
		eq(true, hasPath(fields(8), "ExprArrayTools"));
		eq(true, hasField(fields(5), "iter", "(el : Array<haxe.macro.Expr>, f : (haxe.macro.Expr -> Void)) -> Void"));
		eq(true, hasField(fields(9), "iter", "(el : Array<haxe.macro.Expr>, f : (haxe.macro.Expr -> Void)) -> Void"));
		eq("(el : Array<haxe.macro.Expr>, f : (haxe.macro.Expr -> Void)) -> Void", type(5));
	}

	/**
		import haxe.{-1-}
	**/
	function testImport2(_) {
		eq(true, hasPath(fields(1), "Serializer"));
	}

	/**
		import haxe.Serializer.{-1-}
	**/
	function testImport3(_) {
		eq(true, hasPath(fields(1), "run"));
		eq(true, hasPath(fields(1), "Serializer"));
	}

	/**
		using ha{-1-}xe.{-5-}ma{-2-}cro.{-6-}Exp{-3-}rTools.{-7-}Expr{-4-}ArrayTools;
	**/
	function testUsing1(_) {
		eq(true, hasPath(fields(2), "Serializer"));
		eq(true, hasPath(fields(5), "Serializer"));
		eq(true, hasPath(fields(3), "ExprTools"));
		eq(true, hasPath(fields(6), "ExprTools"));
		eq(true, hasPath(fields(4), "ExprArrayTools"));
		eq(true, hasPath(fields(7), "ExprArrayTools"));
	}

	/**
		using haxe.{-1-}
	**/
	function testUsing2(_) {
		eq(true, hasPath(fields(1), "Serializer"));
	}

	/**
		using haxe.Serializer.{-1-}
	**/
	function testUsing3(_) {
		eq(false, hasPath(fields(1), "run"));
		eq(true, hasPath(fields(1), "Serializer"));
	}

	/**
		import haxe.macro.{-1-}
	**/
	function testIssue6408(_) {
		eq(true, hasPath(fields(1), "Context"));
		eq(false, hasPath(fields(1), "Context.hl"));
	}
}
