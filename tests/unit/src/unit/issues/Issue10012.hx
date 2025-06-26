package unit.issues;

#if !macro
class Issue10012 extends unit.Test {
	function test() {
		var s = format("foo", 1, true, a);
		aeq(['"foo"', '1', 'true', 'a'], s);
	}

	macro static function format(...exprs:haxe.macro.Expr):haxe.macro.Expr;
}
#else
class Issue10012 extends unit.Test {
	macro static function format(...exprs:haxe.macro.Expr):haxe.macro.Expr {
		var strings = exprs.toArray().map(e -> macro $v{haxe.macro.ExprTools.toString(e)});
		return macro $a{strings};
	}
}
#end