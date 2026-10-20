import haxe.macro.Context;

class Macro {
	public static function build():Array<haxe.macro.Expr.Field> {
		trace("build FooData");

		Context.defineType({
			pos : Context.currentPos(),
			name : "FooData",
			pack : ["foo"],
			kind : TDClass(),
			fields : [],
		});

		return null;
	}
}
