import haxe.macro.Context;
import haxe.macro.Expr;

class Macro {
	public static function build():Array<Field> {
		// builds Child, which extends the class being built
		Context.buildType(Context.getType("Child"));
		return null;
	}
}
