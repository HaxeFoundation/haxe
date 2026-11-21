import haxe.macro.Context;
import haxe.macro.Expr;

class Macro3 {
	// calls an inline function whose body failed to type
	public static function build():Array<Field> {
		var r = Helper3.f();
		Context.error('Macro3.build ran code that failed to type: Helper3.f() returned "$r"', Context.currentPos());
		return null;
	}
}
