import haxe.macro.Context;
import haxe.macro.Expr.Field;

class Macro {
	public static macro function build():Array<Field> {
		Context.typeof(macro(null : String));
		return null;
	}
}
