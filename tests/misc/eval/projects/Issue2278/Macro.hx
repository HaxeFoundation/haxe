import haxe.macro.Expr;
import haxe.macro.Context;

class Macro {
	macro static public function call(efun:ExprOf<Int->Int>, eval:Expr):Expr {
		var vfun = Context.eval(efun);
		var vval = Context.eval(eval);
		var r = vfun(vval);
		return macro $v{r};
	}

	macro static public function call2(efun:Int->Int, eval:Int):Expr {
		var r = efun(eval);
		return macro $v{r};
	}

	macro static public function call3(efun:Int->Int, eval:Expr):Expr {
		var r = efun(Context.eval(eval));
		return macro $v{r};
	}
}