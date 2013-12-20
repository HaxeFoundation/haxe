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
	macro static public function buildEnumAbstract():Array<Field> {
		var fields = Context.getBuildFields();
		var a = switch(Context.getLocalClass().get().kind) {
			case KAbstractImpl(a): a;
			case _: throw "";
		}
		var aT = a.get();
		var tThis = aT.type;
		var ctA = TAbstract(a, aT.params.map(function(tp) return tp.t));
		for (field in fields) {
			// allow extra static fields
			if( Lambda.has(field.access,AStatic) )
				continue;
			switch(field.kind) {
				case FVar(t, e):
					field.access = [AStatic,APublic,AInline];
					if (e == null) Context.error("Value required", field.pos);
					var monos = aT.params.map(function(_) return Context.typeof(macro null));
					var tF = ctA.applyTypeParameters(aT.params, monos);
					if (t != null) {
						switch(t.toType().follow()) {
							case t = TAbstract(a2, tl2) if (a.toString() == a2.toString()): tF = t;
							case _: Context.error("Explicit field type must be " + ctA.toString(), field.pos);
						}
					}
					var tE = Context.typeof(e);
					if (!Context.unify(tE, tThis)) Context.error('${tE.toString()} should be ${tThis.toString()}', e.pos);
					field.meta.push({name: ":impl", params: [], pos: field.pos});
					field.meta.push({name: ":enum", params: [], pos: field.pos});
					field.kind = FVar(tF.toComplexType(), macro cast $e);
				case _:
			}
		}
		return fields;
	}

	macro static public function exposeUnderlyingFields(fieldExprs:Array<Expr>):Array<Field> {
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
			case _: Context.error("Underlying type of exposing abstract must be a class", Context.currentPos());
		}
		var isExcludePattern = false;
		var excludeList = new Map();

		var fieldExprs = if (fieldExprs.length == 1) 
		{
			switch (fieldExprs[0].expr) {
				case EUnop(OpNot, _, { expr : EArrayDecl(v) }): 
					if (v.length == 0) {
						Context.error("Non empty excluding List expected", x.pos);
					}
					function getIdent (x) return switch (x.expr) {
						case EConst(CIdent(name)): name;
						case _ : Context.error("List of Identifier expected", x.pos);
					}
					excludeList = [for (x in v) getIdent(x) => true ];
					isExcludePattern = true;
					[];
				case _ : 
					fieldExprs;
			}
		} else {
			fieldExprs;
		}

		function getIdentNamePair(e) return switch(e.expr) {
			case EConst(CIdent(s)): { baseName : s, newName : s };
			case EBinop(OpArrow, { expr : EConst(CIdent(s1))}, { expr: EConst(CIdent(s2))}): { baseName: s1, newName : s2 };
			case _: Context.error("Identifier or (Identifier => Identifier) expected", e.pos);
		}

		var thisType = {name: "this", type: tThis.follow().toComplexType(), opt:false, value: null};


		function toFields(cf:ClassField, oldName:String, newName:String, pos) 
		{
			function mkField (kind:FieldType, ?access:Array<Access>, ?name:String, ?noCompletion = false):Field 
			{
				if (access == null) access = [AInline, APublic, AStatic];
				if (name == null) name = newName;
				var meta = [{name: ":impl", params: [], pos: cf.pos}];
				if (noCompletion) {
					meta.push({name: ":noCompletion", params: [], pos: cf.pos});
				}
				return {
					name: name,
					doc: cf.doc,
					access: access,
					pos: cf.pos,
					meta: meta,
					kind: kind
				}
			}

			var res = [];
			var ct = cf.type.follow();
			
			switch(ct) {
				case TFun(args, ret):
					var args = args.map(function(arg) return {
						name: arg.name,
						opt: arg.opt,
						type: arg.t.toComplexType(),
						value: null
					});
					var expr = macro return this.$oldName($a{args.map(function(arg) return macro $i{arg.name})});
					args.unshift(thisType);
					var k = FieldType.FFun({
						args: args,
						ret: ret.toComplexType(),
						expr: expr,
						params: cf.params.map(function(param) return {
							name: param.name,
							constraints: [],
							params: []
						})
					});
					res.push(mkField(k));
				case _:
					switch (cf.kind) {
						case FVar(read = AccNormal | AccInline | AccCall | AccRequire(_, _), write): // only read
							var writeAccess = write.match(AccNormal | AccInline | AccCall);

							/* add property and getter function */
							var kprop = FieldType.FProp("get_" + newName, writeAccess ? "set_" + newName : "never", cf.type.toComplexType(), null);

							var kget = FieldType.FFun({
								args: [thisType],
								ret: ct.toComplexType(),
								expr: macro return this.$oldName,
								params: cf.params.map(function(param) return {
									name: param.name,
									constraints: [],
									params: []
								})
							});
							
							res.push(mkField(kprop, [APublic, AStatic], newName, false));
							res.push(mkField(kget, [AInline, AStatic], "get_" + newName, true));

							/* add setter function */
							if (writeAccess) {
								var kset = FieldType.FFun({
									args: [thisType, {name: "val", type: null, opt:false, value: null}],
									ret: ct.toComplexType(),
									expr: macro return this.$oldName = val,
									params: cf.params.map(function(param) return {
										name: param.name,
										constraints: [],
										params: []
									})
								});
								var f1 = mkField(kset, [AInline, AStatic], "set_" + newName, true);
								res.push(f1);
							}
						case _ : throw "invalid";
					}
					
				case _: throw "invalid";
			}

			return res;
		}

		/* if we no fields are specified, all fields are forwarded */
		function collectFields () 
		{
			// collect all fields and filter property accessors like get_x, set_x (no need to forward)
			var fields1 = c.fields.get();
			var filter = new Map();
			var temp = [];
			for (f in fields1) {
				if (f.isPublic) {
					temp.push({ expr : { expr : EConst(CIdent(f.name)), pos : Context.currentPos()}, name : f.name });

					switch (f.kind) {
						case FVar(r,w): 
							if (r.match(AccCall)) filter.set("get_" + f.name, true);
							if (w.match(AccCall)) filter.set("set_" + f.name, true);
						case _:
					}
				}
			}
			/* filter fields by name */
			return [for (f in temp) if (!filter.exists(f.name) && !excludeList.exists(f.name)) f.expr];
		}

		var forwardAll = fieldExprs.length == 0 && !isExcludePattern;

		var fieldExprs = if (forwardAll) collectFields() else	fieldExprs;

		var abstractFieldLookup = [for (f in fields) f.name=>true];

		var curFieldLookup = if (forwardAll) [for (f in fields) f.name => true] else new Map();

		for (fieldExpr in fieldExprs) 
		{
			var fieldNames = getIdentNamePair(fieldExpr);
			var baseName = fieldNames.baseName;
			var newName = fieldNames.newName;
			if (!curFieldLookup.exists(newName)) {
				
				var cField = c.findField(baseName, false);
				if (cField == null) Context.error('Underlying type has no field $baseName', fieldExpr.pos);
				if (abstractFieldLookup.exists(fieldNames.newName)) {
					var fieldStr = if (newName != baseName) '$baseName => $newName' else '$newName';
					Context.error('Cannot forward field $fieldStr. Abstract already defines $newName.', fieldExpr.pos);
				}
				
				cField.type = map(cField.type);
				var newFields = toFields(cField, baseName, newName, fieldExpr.pos);
				
				for (f in newFields) {
					fields.push(f);	
				}
			}
			
		}
		return fields;
	}
}
