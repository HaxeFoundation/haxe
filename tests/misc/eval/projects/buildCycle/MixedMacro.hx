import haxe.macro.Context;
import haxe.macro.Expr;

class MixedMacro {
	public static function build():Array<Field> {
		Context.typeof(macro MixedB.f());
		var fields = Context.getBuildFields();
		fields.push((macro class {
			public static var y = 2;
		}).fields[0]);
		return fields;
	}
}
