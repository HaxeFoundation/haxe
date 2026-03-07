import haxe.macro.Expr;
import haxe.macro.Context;

class Build {
	macro static public function build():Array<Field> {
		var field = (macro class X {
			public function test() { }
		}).fields[0];
		var fields = Context.getBuildFields();
		fields.push(field);
		return fields;
	}
}