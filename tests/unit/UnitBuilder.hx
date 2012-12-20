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
			if (file.endsWith(".hxunit")) {
				numFiles++;
				var func = {
					args: [],
					ret: null,
					params: [],
					expr: read(basePath + file)
				}
				ret.push( {
					name: "test" + ~/\./.map(file, function(_) return "_"),
					kind: FFun(func),
					pos: Context.makePosition( { min:0, max:0, file:basePath + file } ),
					access: [APublic],
					doc: null,
					meta: []
				});
			}
		}
		trace("Added " +numFiles + " .hxunit files");
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
				case EBinop(OpEq, e1, e2):
					macro eq($e1, $e2);
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