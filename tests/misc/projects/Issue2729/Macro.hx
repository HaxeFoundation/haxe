import haxe.macro.Context;
import haxe.macro.Expr;

class Macro {
	static function build():Array<Field> {
		var fields = Context.getBuildFields();
		return fields;
	}
}
