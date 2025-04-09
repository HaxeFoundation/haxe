package unit.issues.misc;

class Issue12030Macro {
	public static function build() {
		var fields = haxe.macro.Context.getBuildFields();
		function identity(e)
			return haxe.macro.ExprTools.map(e, identity);
		for (f in fields)
			switch f.kind {
				case FFun(f):
					f.expr = identity(f.expr);
				case _:
			};
		return fields;
	}
}
