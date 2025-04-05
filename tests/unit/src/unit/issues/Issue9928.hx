package unit.issues;
#if !macro
class Issue9928 extends unit.Test {
	function test() {
		var w = Std.random(10);
		eq('hello $w', format('hello $w'));
	}

	macro static function format(s:String):haxe.macro.Expr;
}
#else
class Issue9928 extends unit.Test {
	macro static function format(s:String):haxe.macro.Expr {
		var p = haxe.macro.Context.currentPos();
		var ed:{expr:haxe.macro.Expr.ExprDef} = haxe.macro.MacroStringTools.formatString(s, p);
		return haxe.macro.MacroStringTools.formatString(s, p);
	}
}
#end