import haxe.macro.Context;
import haxe.macro.Expr;

class Macro {
	static function markup():Array<Field> {
		var fields = Context.getBuildFields();
		for (f in fields)
			if (f.name == "render")
				switch f.kind {
					case FFun(fn): fn.expr = macro return $e{fn.expr};
					case _:
				}
		return fields;
	}

	static function check():Array<Field> {
		var fields = Context.getBuildFields();
		fields.push((macro class {
			public var checked = true;
		}).fields[0]);
		return fields;
	}

	static function none():Array<Field> {
		return null;
	}
}
