package js.jquery;

@:noPackageRestrict
#if js extern #end class Helper {
	macro public static function J(exprs:Array<haxe.macro.Expr>) {
		return macro new js.jquery.JQuery($a{exprs});
	}

	#if !macro
	public static var JTHIS(get, null) : js.jquery.JQuery;

	static inline function get_JTHIS() : js.jquery.JQuery {
		return new js.jquery.JQuery(js.Lib.nativeThis);
	}
	#end

	@:allow(js.jquery.JQuery)
	macro static function embed() {
		return if (haxe.macro.Context.defined("embed_js")) {
			macro haxe.macro.Compiler.includeFile("js/jquery/jquery-1.11.3.min.js");
		} else {
			macro {};
		}
	}
}