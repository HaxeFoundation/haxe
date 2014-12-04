package unit.issues.misc;

import haxe.macro.Expr;
import haxe.macro.Context;
import haxe.macro.Type;

using haxe.macro.Tools;

class Issue3183Macro {
	static var tupleMap = new Map();

	macro static public function buildTuple():ComplexType {
		switch(Context.getLocalType()) {
			case TInst(c, args):
				var arity = args.length;
				if (arity == 0) {
					var el = Context.getConstructorArguments();
					if (el != null && el.length > 0) {
						args = [for (e in el) Context.typeof(e)];
						arity = args.length;
					} else {
						return null;
					}
				}
				if (!tupleMap.exists(arity)) {
					tupleMap[arity] = buildTupleType(c.get(), Context.getBuildFields(), arity);
				}
				var ct = tupleMap[arity];
				ct.params = [for (t in args) {
					switch (t) {
						case TInst(_.get().kind => KExpr(e), _):
							TPType(Context.typeof(e).toComplexType());
						case _:
							TPType(t.toComplexType());
					}
				}];
				return TPath(ct);
			case _:
				return Context.error("Class expected", Context.currentPos());
		}
	}

	static function buildTupleType(c:ClassType, fields:Array<Field>, arity:Int) {
		var typeParams = [];
		var tupleFields = [];
		for (i in 0...arity) {
			var fieldName = 'v$i';
			var typeParamName = 'T$i';
			var typeParam = {
				TPath({
					pack: [],
					name: typeParamName,
					sub: null,
					params: []
				});
			}
			typeParams.push({
				name: typeParamName,
				constraints: [],
				params: []
			});
			var field = (macro class X {
				public var $fieldName:$typeParam;
			}).fields[0];
			tupleFields.push(field);
		}
		var constructor = {
			name: "new",
			pos: c.pos,
			access: [APublic, AInline],
			kind: FFun({
				ret: null,
				expr: macro $b{tupleFields.map(function(field) {
					var name = field.name;
					return macro this.$name = $i{name};
				})},
				params: [],
				args: tupleFields.map(function(field) {
					return {
						name: field.name,
						type: null,
						opt: false,
						value: null
					}
				})
			})
		}
		var name = c.name + "_" + arity;
		var tDef = {
			pack: c.pack,
			name: name,
			pos: c.pos,
			kind: TDClass(),
			params: typeParams,
			fields: fields.concat(tupleFields).concat([constructor])
		}
		Context.defineType(tDef);
		return {
			pack: c.pack,
			name: name,
			params: [],
			sub: null
		}
	}
}