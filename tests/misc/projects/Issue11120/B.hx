import haxe.macro.Context;

class B {
	public static function build() {
		Context.typeExpr(macro fromCharCode);
		return null;
	}
}