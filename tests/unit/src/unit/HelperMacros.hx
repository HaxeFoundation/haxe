package unit;

import haxe.macro.Expr;
import haxe.macro.Context.*;
import haxe.macro.TypeTools.*;

class HelperMacros {
	static public macro function getCompilationDate() {
		return macro $v { Std.string(Date.now()) };
	}

	static public macro function typeString(e) {
		var typed = typeExpr(e);
		var s = haxe.macro.TypeTools.toString(typed.t);
		return macro $v{s};
	}

	static public macro function typedAs(actual:haxe.macro.Expr, expected:haxe.macro.Expr) {
		var tExpected = typeof(expected);
		var tActual = typeof(actual);
		return parse("eq('" +Std.string(tActual) + "', '" +Std.string(tExpected) + "')", currentPos());
	}

	static public macro function isNullable(expr:haxe.macro.Expr) {
		var t = typeof(expr);
		function isNullable(t:haxe.macro.Type) {
			return switch (t) {
				case TMono(null): false;
				case TMono(t): isNullable(t.get());
				case TAbstract(_.get() => {pack: [], name: "Null"}, _): true;
				case TLazy(f): isNullable(f());
				case TType(_.get() => td, tl): isNullable(applyTypeParameters(td.type, td.params, tl));
				case TFun(_): false;
				case TAbstract(_.get() => a, _) if (a.meta.has(":coreType")): !a.meta.has(":notNull");
				case TAbstract(_.get() => a, tl): !a.meta.has(":notNull") && isNullable(applyTypeParameters(a.type, a.params, tl));
				case _: true;
			}
		}
		return macro $v{isNullable(t)};
	}

	static public macro function typeError(e:haxe.macro.Expr) {
		var result = try {
			typeof(e);
			"false";
		} catch (e:Dynamic) "true";
		return { pos: currentPos(), expr: haxe.macro.Expr.ExprDef.EConst(haxe.macro.Expr.Constant.CIdent(result)) };
	}

	static public macro function typeErrorText(e:haxe.macro.Expr) {
		var result = try {
			typeof(e);
			null;
		} catch (e:haxe.macro.Expr.Error) e.message;
		return {
			pos: currentPos(),
			expr: if (result == null)
					haxe.macro.Expr.ExprDef.EConst(haxe.macro.Expr.Constant.CIdent("null"))
				else
					haxe.macro.Expr.ExprDef.EConst(haxe.macro.Expr.Constant.CString(result))
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
			typeof(e);
			"no error";
		} catch (e:Dynamic) Std.string(e.message);
		return macro $v{result};
	}

	static public macro function parseAndPrint(s:String) {
		var e = parse(s, currentPos());
		var s2 = new haxe.macro.Printer().printExpr(e);
		return macro eq($v{s}, $v{s2});
	}
}