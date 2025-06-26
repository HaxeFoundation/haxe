/*
 * Copyright (C)2005-2018 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package unit;

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;

using StringTools;

class UnitBuilder {
	static public macro function generateSpec(basePath:String) {
		var ret = [];
		var numFiles = 0;

		function readDir(path, pack:Array<String>) {
			var dir = sys.FileSystem.readDirectory(path);
			path = path.endsWith("\\") || path.endsWith("/") ? path : path + "/";
			for (file in dir) {
				var filePath = path + file;
				if (file.endsWith('.unit.hx')) {
					numFiles++;
					var func = {
						args: [],
						ret: null,
						params: [],
						expr: read(filePath)
					}
					var p = Context.makePosition({min: 0, max: 0, file: filePath + file});
					var field = {
						name: "test",
						kind: FFun(func),
						pos: p,
						access: [APublic],
						doc: null,
						meta: file.endsWith("Utf8.unit.hx") ? [{name: ":haxe.warning", params: [macro "-WDeprecated"], pos: p}] : []
					};
					var pack = ["unit", "spec"].concat(pack);
					var typeName = "Test" + file.substr(0, file.indexOf("."));
					Context.defineModule(pack.join(".") + "." + typeName, [
						{
							pack: pack,
							name: typeName,
							pos: p,
							kind: TDClass({
								pack: ["unit"],
								name: "Test"
							}),
							fields: [field]
						}
					], [
						{
							path: [
								{pos: p, name: "unit"},
								{pos: p, name: "spec"},
								{pos: p, name: "TestSpecification"}
							],
							mode: INormal
						},
						{
							// TODO: import.hx doesn't work for this?
							path: [{pos: p, name: "haxe"}, {pos: p, name: "macro"}, {pos: p, name: "Expr"}],
							mode: INormal
						}
					]);
					var tp:TypePath = {
						pack: pack,
						name: typeName
					}
					ret.push(macro new $tp());
				} else if (sys.FileSystem.isDirectory(filePath)) {
					readDir(filePath, pack.concat([file]));
				} else if (filePath.endsWith('.hx')) {
					Context.error('$filePath: specification tests filenames should end with ".unit.hx"', Context.currentPos());
				}
			}
		}
		readDir(basePath, []);
		// trace("Added " +numFiles + " .unit.hx files");
		return macro $a{ret};
	}

	#if macro
	static function collapseToOrExpr(el:Array<Expr>) {
		return switch (el) {
			case []: throw "";
			case [e]: e;
			case _:
				var e = el.pop();
				{expr: EBinop(OpBoolOr, e, collapseToOrExpr(el)), pos: e.pos}
		}
	}

	static function mkEq(e1, e2, p:Position) {
		function isFloat(e) {
			try
				return switch (Context.follow(Context.typeof(e))) {
					case TAbstract(tr, _):
						tr.get().name == "Float";
					case _:
						false;
				} catch (e:Dynamic) {
				return false;
			}
		}
		var e = switch [isFloat(e1) || isFloat(e2), e2.expr] {
			case [
				_,
				EField({expr: EConst(CIdent("Math" | "math"))}, "POSITIVE_INFINITY" | "NEGATIVE_INFINITY")
			] if (Context.defined("cpp") || Context.defined("php")):
				macro t($e1 == $e2);
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
		var p = Context.makePosition({min: 0, max: 0, file: path});
		var file = sys.io.File.getContent(path);
		var code = Context.parseInlineString("{" + file + "\n}", p);
		function mkBlock(e:Expr) {
			return switch (e.expr) {
				case EBlock(b): b;
				case _: [e];
			}
		}
		function bl(block:Array<Expr>):Array<Expr> {
			var ret = [];
			for (e in block) {
				var e = switch (e.expr) {
					case EBinop(OpEq, e1, {expr: EConst(CIdent("false"))}) | EBinop(OpEq, {expr: EConst(CIdent("false"))}, e1):
						{
							expr: (macro f($e1)).expr,
							pos: e.pos
						}
					case EBinop(OpEq, e1, {expr: EConst(CIdent("true"))}) | EBinop(OpEq, {expr: EConst(CIdent("true"))}, e1):
						{
							expr: (macro t($e1)).expr,
							pos: e.pos
						}
					case EBinop(OpEq, e1, {expr: EArrayDecl(el)}) | EBinop(OpEq, {expr: EArrayDecl(el)}, e1):
						var el2 = [];
						for (i in 0...el.length) {
							var e2 = el[i];
							el2.push(mkEq((macro $e1[$v{i}]), e2, e.pos));
						}
						if (el2.length == 0) mkEq((macro @:pos(e1.pos) $e1.length), (macro 0), e.pos); else macro {$a{el2};};
					case EBinop(OpEq, e1, e2):
						mkEq(e1, e2, e.pos);
					case EBinop(OpNotEq, e1, e2):
						macro @:pos(e.pos) t($e1 != $e2);
					case EBinop(OpGt | OpGte | OpLt | OpLte, _, _):
						{
							expr: (macro t($e)).expr,
							pos: e.pos
						}
					case EThrow(e):
						macro exc(function() $e);
					case EBinop(OpIn, e1, {expr: EArrayDecl(el)}):
						var el2 = [];
						for (e in el)
							el2.push(macro $e1 == $e);
						macro @:pos(e.pos) t(${collapseToOrExpr(el2)});
					case EVars(vl):
						for (v in vl)
							if (v.name == "t" || v.name == "f" || v.name == "eq" || v.name == "neq")
								Context.error('${v.name} is reserved for unit testing', e.pos);
						e;
					case EFor(it, {expr: EBlock(el), pos: p}):
						{expr: EFor(it, {expr: EBlock(bl(el)), pos: p}), pos: e.pos};
					case _:
						e;
				}
				ret.push(e);
			}
			return ret;
		}
		var block = mkBlock(code);
		return macro $b{bl(block)};
	}
	#end
}
