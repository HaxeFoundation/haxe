package haxe;

// couldn't put it into js.jquery package, or else there will be error:
// "You cannot access the js package while in a macro (for js.jquery.JQuery)"
class JQueryHelper {
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