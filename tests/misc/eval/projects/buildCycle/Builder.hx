import haxe.macro.Context;
import haxe.macro.Expr;

class Builder {
	public static function build(other:String):Array<Field> {
		var fields = Context.getBuildFields();
		var n = switch Context.getType(other) {
			case TInst(c, _): c.get().fields.get().length;
			case _: 0;
		}
		fields.push((macro class X {
			public static function count() return $v{n};
		}).fields[0]);
		return fields;
	}
}
