import haxe.macro.Context;
import haxe.macro.Expr;

class LocalFieldsMacro {
	public static function build():Array<Field> {
		// the fields of the class being built are only available through getBuildFields
		Context.getLocalClass().get().fields.get();
		return null;
	}
}
