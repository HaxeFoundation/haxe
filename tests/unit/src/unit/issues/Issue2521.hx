package unit.issues;
import unit.Test;

class Issue2521 extends Test {
	#if !macro
	function test() {
		eq("haxe.macro.Expr", getType("haxe.macro.Expr"));
		eq("haxe.macro.Expr", getType("haxe.macro.Expr.Expr"));
		eq("haxe.macro.ExprOf<Unknown<0>>", getType("haxe.macro.Expr.ExprOf"));
	}
	#end

	macro static function getType(s:String) {
		var t = haxe.macro.Context.getType(s);
		var s = haxe.macro.TypeTools.toString(t);
		return macro $v{s};
	}
}