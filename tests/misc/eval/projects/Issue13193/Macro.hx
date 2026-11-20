import haxe.macro.Context;
import haxe.macro.Expr;

class Macro {
	// calls a function whose body failed to type
	public static function build1():Array<Field> {
		var r = Helper.wrongType();
		Context.error('Macro.build1 ran code that failed to type: Helper.wrongType():Int returned "$r"', Context.currentPos());
		return null;
	}

	public static function build2():Array<Field> {
		var r = Helper.missingField();
		Context.error('Macro.build2 ran code that failed to type: Helper.missingField():Int returned "$r"', Context.currentPos());
		return null;
	}
}
