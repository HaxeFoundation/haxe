package hxjsonast;

#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
using haxe.macro.Tools;

class JsonMacro {
	public static function build(expr:Expr):Expr {
		return switch expr.expr {
			case EConst(CString(s)):
				mkJsonExpr(macro JString($expr), expr.pos);
			case EConst(CIdent("true" | "false")):
				mkJsonExpr(macro JBool($expr), expr.pos);
			case EConst(CIdent("null")):
				mkJsonExpr(macro JNull, expr.pos);
			case EConst(CInt(s) | CFloat(s)):
				mkJsonExpr(macro JNumber($v{s}), expr.pos);
			case EBlock([]):
				mkJsonExpr(macro JObject([]), expr.pos);
			case EObjectDecl(fields):
				var fieldsExpr = macro $a{fields.map(f -> macro {
					name: $v{f.field},
					namePos: ${mkPosExpr(f.expr.pos)},
					value: ${build(f.expr)}
				})};
				mkJsonExpr(macro JObject($fieldsExpr), expr.pos);
			case EArrayDecl(values):
				var arrayExpr = macro $a{values.map(build)};
				mkJsonExpr(macro JArray($arrayExpr), expr.pos);
			case _:
				throw new Error("Unsupported JSON expression", expr.pos);
		}
	}

	static function mkJsonExpr(valueExpr:Expr, pos:Position) {
		return macro new hxjsonast.Json($valueExpr, ${mkPosExpr(pos)});
	}

	static function mkPosExpr(pos:Position) {
		var pos = haxe.macro.Context.getPosInfos(pos);
		return macro new hxjsonast.Position($v{pos.file}, $v{pos.min}, $v{pos.max});
	}
}
#end
