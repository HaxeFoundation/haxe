package;

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;

class TestBuilder {
	public static function build():Array<Field> {
		var fields:Array<Field> = Context.getBuildFields();
		var classType:ClassType;
		switch (Context.getLocalType()) {
			case TInst(r, _):
				classType = r.get();
			case _:
		}
		classType.superClass.t.get().fields.get();

		return fields;
	}
}
