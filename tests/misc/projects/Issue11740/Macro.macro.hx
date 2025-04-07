import haxe.macro.Context;

class Macro {
	public static function build() {
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
