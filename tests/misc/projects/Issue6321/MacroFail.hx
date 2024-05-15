class MacroFail {
	static function doStuff() {
		switch (haxe.macro.Context.getType("B")) {
			case TInst(_.get() => cl, _):
				for (field in cl.fields.get()) {
					trace(field.name, field.expr()?.pos);
				}
			default:
				throw false;
		}
	}
}
