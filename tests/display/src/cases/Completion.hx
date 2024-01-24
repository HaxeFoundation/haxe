package cases;

class Completion extends DisplayTestCase {
	/**
		var s = { foo: 1 };
		s.{-1-}
	**/
	@:funcCode function testHaxeUnitPort1() {
		eq(true, hasField(fields(pos(1)), "foo", "Int"));
	}

	/**
		var s = { foo: 1 };
		for (k in [s].iterator()) {
			k.{-1-}
		}
	**/
	@:funcCode function testHaxeUnitPort2() {
		eq(true, hasField(fields(pos(1)), "foo", "Int"));
	}

	/**
		var f = function():Iterator<{foo:Int}> {
			return [s].iterator();
		};
		var s = { foo: 1 };
		for (k in f()) {
			k.{-1-}
		}
	**/
	@:funcCode function testHaxeUnitPort3() {
		eq(true, hasField(fields(pos(1)), "foo", "Int"));
	}

	/**
		var x:haxe.macro.{-1-}
	**/
	@:funcCode function testHaxeUnitPort4() {
		eq(true, hasPath(fields(pos(1)), "Expr"));
		DisplayTestContext.runHaxe(['haxe.macro.Expr']);
		eq(true, hasPath(fields(pos(1)), "Expr"));
	}

	/**
		var x:haxe.macro.Expr.{-1-}
	**/
	@:funcCode function testHaxeUnitPort5() {
		eq(true, hasPath(fields(pos(1)), "ExprDef"));
		DisplayTestContext.runHaxe(['haxe.macro.Expr']);
		eq(true, hasPath(fields(pos(1)), "ExprDef"));
	}

	/**
		haxe.Json.{-1-}
	**/
	@:funcCode function testStaticField() {
		eq(true, hasPath(fields(pos(1)), "stringify"));
		DisplayTestContext.runHaxe(['haxe.Json']);
		eq(true, hasPath(fields(pos(1)), "stringify"));
	}

	/**
		var s = { foo: 1 };
		s.{-1-}f{-2-}o{-3-}o{-4-}
	**/
	@:funcCode function testNonDotCompletion1() {
		eq(true, hasField(fields(pos(1)), "foo", "Int"));
		eq(true, hasField(fields(pos(2)), "foo", "Int"));
		eq(true, hasField(fields(pos(3)), "foo", "Int"));
		eq(true, hasField(fields(pos(4)), "foo", "Int"));
	}

	/**
		var s = { foo: 1 };
		"foo".
		wtf
		var lol
		miauga(
		notArray[
		[
		(
		{
		{
			obj:
				obj
					s.{-1-}
	**/
	@:funcCode function testDrunkAst1() {
		eq(true, hasField(fields(pos(1)), "foo", "Int"));
	}
}
