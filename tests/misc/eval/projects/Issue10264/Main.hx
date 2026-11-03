function main() {
	var d:Dynamic = null;
	var v:MacroFrom = d;
}

abstract MacroFrom(String) to String {
	@:from
	public static macro function fromExpr(e:haxe.macro.Expr)
		return macro ($e:Main.MacroFrom);
}