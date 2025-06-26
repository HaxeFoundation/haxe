#if macro
import haxe.macro.Compiler;
import haxe.macro.Context;
import haxe.macro.Expr;

class Macro {
	public static function initMacro() {
		Compiler.addGlobalMetadata("Main", "@:build(Macro.instrumentFields())", true, true, false);
	}

	static function instrumentFields():Null<Array<Field>> {
		var fields:Array<Field> = Context.getBuildFields();
		for (field in fields) {
			switch (field.kind) {
				case FFun(f):
					if (f.expr == null) {
						continue;
					}
					switch (f.expr.expr) {
						case EBlock(exprs):
							exprs.unshift(macro trace($v{field.name}));
						case _:
					}
				case _:
			}
		}
		return fields;
	}
}
#end
