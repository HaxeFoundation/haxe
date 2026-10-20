import haxe.macro.Context;

class B {
	public static function build():Array<haxe.macro.Expr.Field> {
		Context.typeExpr(macro fromCharCode);
		return null;
	}
}