import haxe.macro.Context;
import haxe.macro.Expr;

class Macro {
	public static function addMeta():Array<Field> {
		Context.getLocalClass().get().meta.add(":built", [], Context.currentPos());
		var child = switch Context.getType("Main.Child") {
			case TInst(c, _): c.get();
			case _: throw "unexpected";
		}
		var info = child.name + " extends " + child.superClass.t.get().name;
		var fields = Context.getBuildFields();
		fields.push((macro class {
			public static function info() return $v{info};
		}).fields[0]);
		return fields;
	}

	public static function readMeta():Array<Field> {
		var t = Context.getType("Main.Base");
		var c = switch t {
			case TInst(c, _): c;
			case _: throw "unexpected";
		}
		// declared metadata only, unless this expansion builds the class
		var before = c.get().meta.has(":built");
		Context.buildType(t);
		var after = c.get().meta.has(":built");
		var info = 'before buildType: $before, after: $after';
		var fields = Context.getBuildFields();
		fields.push((macro class {
			public static function info() return $v{info};
		}).fields[0]);
		return fields;
	}
}
