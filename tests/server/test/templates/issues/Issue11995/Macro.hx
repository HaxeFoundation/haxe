import haxe.macro.Context;
using Lambda;

class Macro {
	public static function bar() {
		var fields = Context.getBuildFields();

		switch fields.find(f -> f.name == "foo").kind {
			case FFun(f):
				f.expr = macro {
					var baz:String = "hello";
					${f.expr}
				};

			default: throw "assert";
		}

		return fields;
	}
}
