import haxe.macro.Context;
import haxe.macro.Expr;

class PropertyMacro {
	public static macro function addIntProperty(name:String):Array<Field> {
		final fields = Context.getBuildFields();

		final privateField:Field = {
			name: name,
			access: [APublic],
			meta: [{
				name: ":isVar",
				pos: Context.currentPos()
			}],
			kind: FProp("get", "private set", macro : Int, null),
			pos: Context.currentPos()
		};

		final getterMethod = {
			name: "get_" + name,
			access: [],
			kind: FFun({
				args: [],
				ret: macro : Int,
				expr: macro return this.$name
			}),
			pos: Context.currentPos()
		};

		final setterMethod = {
			name: "set_" + name,
			access: [],
			kind: FFun({
				args: [{ name: "value", type: macro : Int }],
				ret: macro : Int,
				expr: macro return this.$name = value
			}),
			pos: Context.currentPos()
		};

		fields.push(privateField);
		fields.push(getterMethod);
		fields.push(setterMethod);

		return fields;
	}
}
