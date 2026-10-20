function build() {
	// Whoops, not returning anything
}

function build2():Array<haxe.macro.Expr.Field> {
	haxe.macro.Context.error("Abort", haxe.macro.Context.currentPos());
	return null;
}
