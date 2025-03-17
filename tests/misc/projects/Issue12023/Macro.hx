import haxe.macro.Context;
import haxe.macro.Expr.Field;
import haxe.macro.Type.ClassType;

using haxe.macro.TypeTools;

class Macro {
	public static macro function build():Array<Field> {
		var fields:Array<Field> = Context.getBuildFields();
		var classType:ClassType = Context.getLocalClass().get();

		if (classType.isAbstract) {
			fields.push({
				name: "someFieldToAbstractClass",
				pos: Context.currentPos(),
				kind: FVar(macro :Int, macro $v{1})
			});
		}

		return fields;
	}
}
