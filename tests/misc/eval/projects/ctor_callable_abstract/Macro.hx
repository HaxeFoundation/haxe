import haxe.macro.Context;
import haxe.macro.Expr;

class Macro {
	macro public static function build():Array<Field> {
		var fields = Context.getBuildFields();
		fields.push({
			name: "new",
			access: [APublic],
			kind: FVar(macro :ContextProvider, null),
			pos: (macro 0).pos
		});
		return fields;
	}
}
