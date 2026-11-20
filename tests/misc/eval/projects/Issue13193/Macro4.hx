import haxe.macro.Context;
import haxe.macro.Expr;

class Macro4 {
	// calls a function with an argument type that failed to load
	public static function build():Array<Field> {
		var r = Helper4.f();
		Context.error('Macro4.build ran code that failed to type: Helper4.f() returned "$r"', Context.currentPos());
		return null;
	}
}
