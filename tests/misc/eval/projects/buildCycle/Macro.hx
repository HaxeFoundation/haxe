import haxe.macro.Context;
import haxe.macro.Expr;

class Macro {
	public static function build():Array<Field> {
		// forces the build of Child, which extends the class being built
		switch Context.getType("Child") {
			case TInst(c, _): c.get();
			case _:
		}
		return null;
	}
}
