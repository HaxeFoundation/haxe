package cases;

class Issue7703 extends DisplayTestCase {
	/**
		class Main {
		public static function main() {
			f{-1-}oo({-2-}"");
			{-3-}
		}

		static macro function foo(s:String) {
			return macro {};
		}
		}
	**/
	function test() {
		var expectedType = "(s : String) -> haxe.macro.Expr";
		var fields = toplevel(pos(1));
		eq(true, hasToplevel(fields, "static", "foo", expectedType));
		eq(expectedType, type(pos(1)));
		sigEq(0, [["s:String"]], signature(pos(2)));
	}
}
