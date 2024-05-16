function build() {
	// Whoops, not returning anything
}

function build2() {
	haxe.macro.Context.error("Abort", haxe.macro.Context.currentPos());
	return null;
}
