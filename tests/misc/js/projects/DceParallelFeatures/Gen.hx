import haxe.macro.Context;
import haxe.macro.Expr;

class Gen {
	public static function build() {
		var fields = Context.getBuildFields();
		var calls = [];
		for (i in 0...200) {
			var name = 'f$i';
			var feature = 'Feature$i';
			fields.push({
				name: name,
				pos: Context.currentPos(),
				access: [APublic, AStatic],
				kind: FFun({args: [], ret: macro :Int, expr: macro return untyped __define_feature__($v{feature}, $v{i})})
			});
			calls.push(macro $i{name}());
		}
		fields.push({
			name: "useAll",
			pos: Context.currentPos(),
			access: [APublic, AStatic],
			kind: FFun({args: [], ret: null, expr: macro $b{calls}})
		});
		return fields;
	}
}
