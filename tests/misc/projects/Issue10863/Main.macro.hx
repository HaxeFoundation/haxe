import haxe.macro.Context;
import haxe.macro.Expr;

macro function foo():Expr {
	Context.warning("from macro", (macro 0).pos);
	Context.warning("from non macro", Context.currentPos());
	return macro null;
}
