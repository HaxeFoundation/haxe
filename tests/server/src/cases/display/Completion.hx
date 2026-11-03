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
			extern inline overload static function foo(a:haxe.ds.Option<Bool>, cb:() -> Void) {};
			extern inline overload static function foo(a:Int, cb:() -> Void) {};

			extern inline overload static function foo2(a:Int, cb:() -> Void) {};
			extern inline overload static function foo2(a:haxe.ds.Option<Bool>, cb:() -> Void) {};

			static function main() {
				foo({-1-}
				foo(R{-2-}
				foo2({-3-}
				foo2(R{-4-}
			}
		}
	**/
	function testIssue13012(_) {
		eq(true, hasField(fields(1), "Some", null, "enum"));
		eq(true, hasField(fields(1), "None", null, "enum"));
		eq(true, hasField(fields(2), "Some", null, "enum"));
		eq(true, hasField(fields(2), "None", null, "enum"));

		// This is not a specification, just the current state: the first overload that is tried
		// determines the completion, so the Option overload's enum constructors are not offered.
		eq(false, hasField(fields(3), "Some", null, "enum"));
		eq(false, hasField(fields(3), "None", null, "enum"));
		eq(false, hasField(fields(4), "Some", null, "enum"));
		eq(false, hasField(fields(4), "None", null, "enum"));
	}

	/**
		class Main {
			@:overload(function(a:Int, cb:() -> Void):Void {})
			static function foo(a:haxe.ds.Option<Bool>, cb:() -> Void) {};

			static function main() {
				foo({-1-}
				foo(R{-2-}
			}
		}
	**/
	function testIssue13012_metadata(_) {
		eq(true, hasField(fields(1), "Some", null, "enum"));
		eq(true, hasField(fields(1), "None", null, "enum"));
		eq(true, hasField(fields(2), "Some", null, "enum"));
		eq(true, hasField(fields(2), "None", null, "enum"));
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
