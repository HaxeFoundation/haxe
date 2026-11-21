import haxe.macro.Context;
import haxe.macro.Expr;

class Macro5 {
	// reads an instance field whose initializer failed to type
	public static function build():Array<Field> {
		var r = new Helper5().v;
		Context.error('Macro5.build ran code that failed to type: new Helper5().v returned "$r"', Context.currentPos());
		return null;
	}
}
