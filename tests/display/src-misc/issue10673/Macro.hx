package issue10673;

class Macro {
	public static function build() {
		var fields = haxe.macro.Context.getBuildFields();
		for (field in fields) {
			switch field.kind {
				case FVar(_, e):
					field.kind = FVar(TPath({
						pack: ["std"],
						name: "StdTypes",
						sub: "Int"
					}), e);
				case _:
			}
		}
		return fields;
	}
}
