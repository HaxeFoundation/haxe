package unit;

import haxe.macro.Expr;

class HelperMacros {
	static public macro function getCompilationDate() {
		return macro $v { Std.string(Date.now()) };
	}

	static public macro function typedAs(actual:haxe.macro.Expr, expected:haxe.macro.Expr) {
		var tExpected = haxe.macro.Context.typeof(expected);
		var tActual = haxe.macro.Context.typeof(actual);
		return haxe.macro.Context.parse("eq('" +Std.string(tActual) + "', '" +Std.string(tExpected) + "')", haxe.macro.Context.currentPos());
	}

	static public macro function typeError(e:haxe.macro.Expr) {
		var result = try {
			haxe.macro.Context.typeof(e);
			"false";
		} catch (e:Dynamic) "true";
		return { pos: haxe.macro.Context.currentPos(), expr: haxe.macro.Expr.ExprDef.EConst(haxe.macro.Expr.Constant.CIdent(result)) };
	}

	static public macro function typeErrorText(e:haxe.macro.Expr) {
		var result = try {
			haxe.macro.Context.typeof(e);
			null;
		} catch (e:haxe.macro.Expr.Error) e.message;
		return {
			pos: haxe.macro.Context.currentPos(),
			expr: if (result == null)
					haxe.macro.Expr.ExprDef.EConst(haxe.macro.Expr.Constant.CIdent("null"))
				else
					haxe.macro.Expr.ExprDef.EConst(haxe.macro.Expr.Constant.CString(result,haxe.macro.Expr.StringKind.Double))
		};
	}

	static public macro function getMeta(e) {
		switch(e.expr) {
			case EMeta(m, _):
				return macro { name: $v{m.name}, args: $a{m.params} };
			default:
				return macro report("Metadata expected");
		}
	}

	static public macro function getErrorMessage(e:Expr) {
		var result = try {
			haxe.macro.Context.typeof(e);
			"no error";
		} catch (e:Dynamic) Std.string(e.message);
		return macro $v{result};
	}

	static public macro function parseAndPrint(s:String) {
		var e = haxe.macro.Context.parse(s, haxe.macro.Context.currentPos());
		var s2 = new haxe.macro.Printer().printExpr(e);
		return macro eq($v{s}, $v{s2});
	}
}