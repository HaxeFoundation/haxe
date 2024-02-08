import haxe.macro.Context;
import haxe.macro.Expr;

class Builder {
	macro static public function build():Array<Field> {
		var fields = Context.getBuildFields();
		fields.push((macro class Dummy {
			static function generated() {}
		}).fields[0]);
		return fields;
	}
}