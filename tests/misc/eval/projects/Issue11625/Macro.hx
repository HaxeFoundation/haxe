function build():Array<haxe.macro.Expr.Field> {
	var cls = haxe.macro.Context.getLocalClass().get();
	cls.meta.add(":using", [macro Main.Extensions], cls.pos);
	return null;
}
