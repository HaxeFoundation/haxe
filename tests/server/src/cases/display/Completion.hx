package cases.display;

class Completion extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var s = { foo: 1 };
				s.{-1-}
			}
		}
	**/
	function testHaxeUnitPort1(_) {
		eq(true, hasField(fields(1), "foo", "Int"));
	}

	/**
		class Main {
			static function main() {
				var s = { foo: 1 };
				for (k in [s].iterator()) {
					k.{-1-}
				}
			}
		}
	**/
	function testHaxeUnitPort2(_) {
		eq(true, hasField(fields(1), "foo", "Int"));
	}

	/**
		class Main {
			static function main() {
				var f = function():Iterator<{foo:Int}> {
					return [s].iterator();
				};
				var s = { foo: 1 };
				for (k in f()) {
					k.{-1-}
				}
			}
		}
	**/
	function testHaxeUnitPort3(_) {
		eq(true, hasField(fields(1), "foo", "Int"));
	}

	/**
		class Main {
			static function main() {
				var x:haxe.macro.{-1-}
			}
		}
	**/
	function testHaxeUnitPort4(_) {
		eq(true, hasPath(fields(1), "Expr"));
		runHaxe(["haxe.macro.Expr"]);
		vfs.putContent("Main.hx", source);
		eq(true, hasPath(fields(1), "Expr"));
	}

	/**
		class Main {
			static function main() {
				var x:haxe.macro.Expr.{-1-}
			}
		}
	**/
	function testHaxeUnitPort5(_) {
		eq(true, hasPath(fields(1), "ExprDef"));
		runHaxe(["haxe.macro.Expr"]);
		vfs.putContent("Main.hx", source);
		eq(true, hasPath(fields(1), "ExprDef"));
	}

	/**
		class Main {
			static function main() {
				haxe.Json.{-1-}
			}
		}
	**/
	function testStaticField(_) {
		eq(true, hasPath(fields(1), "stringify"));
		runHaxe(["haxe.Json"]);
		vfs.putContent("Main.hx", source);
		eq(true, hasPath(fields(1), "stringify"));
	}

	/**
		class Main {
			static function main() {
				var s = { foo: 1 };
				s.{-1-}f{-2-}o{-3-}o{-4-}
			}
		}
	**/
	function testNonDotCompletion1(_) {
		eq(true, hasField(fields(1), "foo", "Int"));
		eq(true, hasField(fields(2), "foo", "Int"));
		eq(true, hasField(fields(3), "foo", "Int"));
		eq(true, hasField(fields(4), "foo", "Int"));
	}

	/**
		class Main {
			static function main() {
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
			}
		}
	**/
	function testDrunkAst1(_) {
		eq(true, hasField(fields(1), "foo", "Int"));
	}
}
