import haxe.macro.Context;
import haxe.macro.Expr;

macro function foo():Expr {
	Context.warning("from Main.macro.hx", (macro 0).pos);
	Context.warning("from Main.js.hx", Context.currentPos());
	return macro null;
}
