import haxe.macro.Context;
import haxe.macro.Expr;

class Macro {
	public static function invalidField():Array<Field> {
		var fields = Context.getBuildFields();
		fields.push({
			name: "0",
			kind: FVar(macro:String, null),
			pos: Context.currentPos()
		});
		return fields;
	}
}
