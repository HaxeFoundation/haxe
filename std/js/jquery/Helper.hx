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
}