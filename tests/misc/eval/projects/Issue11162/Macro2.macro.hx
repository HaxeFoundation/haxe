import haxe.macro.Expr;

class Macro2 {
	public static function build() {
		var pos = (macro 0).pos;
		var e = {expr: EBlock([
			{expr: EVars([{name: "a", type: null, expr: null}]), pos: pos},
			{expr: EBinop(OpAssign, {expr: EConst(CIdent("a")), pos: pos}, macro main()), pos: pos}
		]), pos: pos};

		return haxe.macro.Context.getBuildFields().concat((macro class A {
			function bar() $e;
		}).fields);
	}
}
