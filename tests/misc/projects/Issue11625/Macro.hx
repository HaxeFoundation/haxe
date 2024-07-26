function build() {
	var cls = haxe.macro.Context.getLocalClass().get();
	cls.meta.add(":using", [macro Main.Extensions], cls.pos);
	return null;
}
