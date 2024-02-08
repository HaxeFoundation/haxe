import haxe.macro.Context;
import haxe.macro.Expr;

class Macro {
	public static function invalidField():Array<Field> {
		var fields = Context.getBuildFields();
		function addField(name:String) {
			fields.push({
				name: name,
				kind: FVar(macro:String, null),
				pos: Context.currentPos()
			});
		}
		addField("0");
		addField("this");
		return fields;
	}
}
