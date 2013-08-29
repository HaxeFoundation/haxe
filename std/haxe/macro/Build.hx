/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package haxe.macro;

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;

using haxe.macro.Tools;

class Build {
	macro static public function buildFakeEnum():Array<Field> {
		var fields = Context.getBuildFields();
		var a = switch(Context.getLocalClass().get().kind) {
			case KAbstractImpl(a): a;
			case _: throw "";
		}
		var tThis = a.get().type;
		var ctA = TAbstract(a, []).toComplexType();
		for (field in fields) {
			field.access = [AStatic,APublic,AInline];
			switch(field.kind) {
				case FVar(t, e):
					if (e == null) Context.error("Value required", field.pos);
					var tE = Context.typeof(e);
					if (!Context.unify(tE, tThis)) Context.error('${tE.toString()} should be ${tThis.toString()}', e.pos);
					field.kind = FVar(ctA, macro cast $e);
				case _:
			}
		}
		return fields;
	}

	macro static public function forwardAbstractFields(fieldExprs:Array<Expr>):Array<Field> {
		var fields = Context.getBuildFields();
		var a = switch(Context.getLocalClass().get().kind) {
			case KAbstractImpl(a): a;
			case _: throw "";
		}
		var tThis = a.get().type;
		var map:Type->Type = function(t) return t;
		var c = switch(tThis.follow()) {
			case TInst(c, tl):
				var c = c.get();
				if (tl.length > 0) map = function(t) {
					var t2 = t.applyTypeParameters(c.params, tl);
					return t2;
				}
				c;
			case _: Context.error("Underlying type of forwarding abstract must be a class", Context.currentPos());
		}
		function getIdentName(e) return switch(e.expr) {
			case EConst(CIdent(s)): s;
			case _: Context.error("Identifier expected", e.pos);
		}
		function toField(cf:ClassField) {
			var name = cf.name;
			return {
				name: name,
				doc: cf.doc,
				access: [AStatic, APublic, AInline],
				pos: cf.pos,
				meta: [{name: ":impl", params: [], pos: cf.pos}],
				kind: switch(cf.type.follow()) {
					case TFun(args, ret):
						var args = args.map(function(arg) return {
							name: arg.name,
							opt: arg.opt,
							type: arg.t.toComplexType(),
							value: null
						});
						var expr = macro return this.$name($a{args.map(function(arg) return macro $i{arg.name})});
						args.unshift({name: "this", type: null, opt:false, value: null});
						FFun({
							args: args,
							ret: ret.toComplexType(),
							expr: expr,
							params: cf.params.map(function(param) return {
								name: param.name,
								constraints: [],
								params: []
							})
						});
					case _: throw "";
				}
			}
		}
		for (fieldExpr in fieldExprs) {
			var fieldName = getIdentName(fieldExpr);
			var cField = c.findField(fieldName, false);
			if (cField == null) Context.error('Underlying type has no field $fieldName', fieldExpr.pos);
			switch(cField.kind) {
				case FMethod(_):
				case _: Context.error("Only function fields can be forwarded", fieldExpr.pos);
			}
			cField.type = map(cField.type);
			var field = toField(cField);
			fields.push(field);
		}
		return fields;
	}
}