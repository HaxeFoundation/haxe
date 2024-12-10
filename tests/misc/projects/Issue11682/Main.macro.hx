macro function myMacro(expr:haxe.macro.Expr):haxe.macro.Expr {
	var texpr = haxe.macro.Context.typeExpr(expr);
	switch texpr.expr {
		case TLocal(tvar) if (tvar.isStatic):
		case _:
			haxe.macro.Context.error("Expected local static", expr.pos);
	}

	return macro null;
}
