class Macro {
	static function doStuff() {
		haxe.macro.Context.onAfterInitMacros(() -> {
			switch (haxe.macro.Context.getType("B")) {
				case TInst(_.get() => cl, _):
					for (field in cl.fields.get()) {
						trace(field.name, field.expr()?.pos);
					}
				default:
					throw false;
			}
		});
	}
}
