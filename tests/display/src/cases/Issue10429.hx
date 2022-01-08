package cases;

class Issue10429 extends DisplayTestCase {
	/**
		import haxe.macro.Expr;

		typedef ArgType = {
			var f:String;
		}

		class Main {
			public static function main() {
				f{-1-}oo({-2-});
				{-3-}
			}

			static macro function foo(s:String, e:ExprOf<String>, o:ArgType) {
				return macro {};
			}
		}
	**/
	function test() {
		var expectedType = "(s : String, e : String, o : cases.ArgType) -> haxe.macro.Expr";
		var fields = toplevel(pos(1));
		eq(true, hasToplevel(fields, "static", "foo", expectedType));
		eq(expectedType, type(pos(1)));
		sigEq(0, [["s:String", "e:String", "o:cases.ArgType"]], signature(pos(2)));
	}
}
