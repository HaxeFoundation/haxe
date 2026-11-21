import haxe.macro.Context;
import haxe.macro.Expr;

class Macro6 {
	// reads a static variable whose initializer failed to type
	public static function build():Array<Field> {
		var r = Helper6.v;
		Context.error('Macro6.build ran code that failed to type: Helper6.v returned "$r"', Context.currentPos());
		return null;
	}
}
