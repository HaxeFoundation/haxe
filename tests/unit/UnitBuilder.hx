package unit;

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
using StringTools;

class UnitBuilder {
	
	@:macro static public function build(basePath:String):Array<Field> {
		basePath = basePath.endsWith("\\") || basePath.endsWith("/") ? basePath : basePath + "/";
		var dir = sys.FileSystem.readDirectory(basePath);
		var ret = Context.getBuildFields();
		var numFiles = 0;
		for (file in dir) {
			if (file.endsWith(".unit.hx")) {
				numFiles++;
				var func = {
					args: [],
					ret: null,
					params: [],
					expr: read(basePath + file)
				}
				ret.push( {
					name: "test" + ~/\./g.map(file, function(_) return "_"),
					kind: FFun(func),
					pos: Context.makePosition( { min:0, max:0, file:basePath + file } ),
					access: [APublic],
					doc: null,
					meta: []
				});
			}
		}
		trace("Added " +numFiles + " .unit.hx files");
		return ret;
	}
	
	#if macro
	static function collapseToOrExpr(el:Array<Expr>) {
		return switch(el) {
			case []: throw "";
			case [e]: e;
		case _:
			var e = el.pop();
			{ expr: EBinop(OpBoolOr, e, collapseToOrExpr(el)), pos: e.pos }
		}
	}
	
	static function mkEq(e1, e2, p) {
		function isFloat(e) {
			try return switch(Context.typeof(e)) {
				case TAbstract(tr, _):
					tr.get().name == "Float";
				case _:
					false;
			} catch (e:Dynamic) {
				return false;
			}
		}
		var e = switch [isFloat(e1) || isFloat(e2), e2.expr] {
			// hell yeah
			case [true, EField( { expr:EConst(CIdent("Math")) }, "POSITIVE_INFINITY" | "NEGATIVE_INFINITY")] if (Context.defined("cpp") || Context.defined("php")):
				macro Math.isFinite($e1);
			case [true, _]:
				macro feq($e1, $e2);
			case _:
				macro eq($e1, $e2);
		}
		return {
			expr: e.expr,
			pos: p
		}
	}
	static public function read(path:String) {
		var p = Context.makePosition( { min:0, max:0, file:path } );
		var file = sys.io.File.getContent(path);
		var code = Context.parseInlineString("{" + file + "}", p);
		var block = switch(code.expr) {
			case EBlock(b): b;
			case _: throw "false";
		}
		var ret = [];
		for (e in block) {
			var e = switch(e.expr) {
				case EBinop(OpEq, e1, { expr: EConst(CIdent("false")) } )
				| EBinop(OpEq, { expr: EConst(CIdent("false")) }, e1):
					{
						expr: (macro f($e1)).expr,
						pos: e.pos
					}
				case EBinop(OpEq, e1, { expr: EConst(CIdent("true")) } )
				| EBinop(OpEq, { expr: EConst(CIdent("true")) }, e1):
					{
						expr: (macro t($e1)).expr,
						pos: e.pos
					}
				case EBinop(OpEq, e1, { expr: EArrayDecl(el) } )
				| EBinop(OpEq, { expr: EArrayDecl(el) }, e1 ):
					var el2 = [];
					for (i in 0...el.length) {
						var e2 = el[i];
						el2.push(mkEq((macro $e1[$(i)]), e2, e.pos));
					}
					if (el2.length == 0)
						mkEq((macro $e1.length), (macro 0), e.pos);
					else
						macro { $[el2]; };
				case EBinop(OpEq, e1, e2):
					mkEq(e1, e2, e.pos);
				case EThrow(e):
					macro exc(function() $e);
				case EIn(e1, {expr:EArrayDecl(el) }):
					var el2 = [];
					for (e in el)
						el2.push(macro $e1 == $e);
					collapseToOrExpr(el2);
				case _:
					e;
			}
			ret.push(e);
		}
		return macro { $[ret]; };
	}
	#end
}