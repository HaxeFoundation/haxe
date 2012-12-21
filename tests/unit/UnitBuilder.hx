package unit;

import haxe.macro.Context;
import haxe.macro.Expr;
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
	
	static function collapseToAndExpr(el:Array<Expr>) {
		return switch(el) {
			case []: throw "";
			case [e]: e;
		case _:
			var e = el.pop();
			{ expr: EBinop(OpBoolAnd, e, collapseToAndExpr(el)), pos: e.pos }
		}
	}	
	
	static function mkEq(e1, e2, p) {
		var e = macro eq($e1, $e2);
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
				case EBinop(OpEq, e, { expr: EConst(CIdent("false")) } )
				| EBinop(OpEq, { expr: EConst(CIdent("false")) }, e):
					macro f($e);					
				case EBinop(OpEq, e, { expr: EConst(CIdent("true")) } )
				| EBinop(OpEq, { expr: EConst(CIdent("true")) }, e):
					macro t($e);
				case EBinop(OpEq, e, { expr: EArrayDecl([]) } )
				| EBinop(OpEq, { expr: EArrayDecl([]) }, e ):
					var ef = { expr: EField(e, "length"), pos: e.pos };
					macro eq($ef, 0);
				case EBinop(OpEq, e, { expr: EArrayDecl(el) } )
				| EBinop(OpEq, { expr: EArrayDecl(el) }, e ):
					var el2 = [];
					for (i in 0...el.length) {
						var e1 = el[i];
						el2.push(mkEq((macro $e[$(i)]), e1, e1.pos));
					}
					if (el2.length == 0)
						macro eq($e.length, 0);
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