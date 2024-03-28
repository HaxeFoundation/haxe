package utils.macro;

import haxe.macro.Expr;
import haxe.macro.Context;
import haxe.macro.Type;

using StringTools;

class TestBuilder {
	static public function build(fields:Array<Field>):Array<Field> {
		var removedFields = [];
		var newFields = [];

		for (field in fields) {
			if (!field.name.startsWith("test")) {
				continue;
			}
			switch (field.kind) {
				case FFun(f) if (field.meta.exists(m -> m.name == ":async")):
					// Async is already manually handled, nothing to do

				case FFun(f):
					var variants = field.meta.filter(m -> m.name == ":variant");
					if (variants.length == 0) {
						makeAsyncTest(field);
					} else {
						// TODO: support functions that define their own async arg (not named `_` or `async`)
						var args = f.args.copy();
						f.args = [];
						makeAsyncTest(field);

						// Ignore original field; generate variants instead
						removedFields.push(field);

						for (variant in variants) {
							if (variant.params.length == 0) {
								Context.error('Unexpected amount of variant parameters.', variant.pos);
							}

							var nameParam = variant.params.shift();
							var name:String = try haxe.macro.ExprTools.getValue(nameParam) catch(e) {
								Context.error('Variant first parameter should be a String (variant name)', nameParam.pos);
							};

							var inits = [for (arg in args) {
								var name = arg.name;
								var ct = arg.type;

								if (variant.params.length == 0) {
									Context.error('Unexpected amount of variant parameters.', variant.pos);
								}

								var param = variant.params.shift();
								macro @:pos(param.pos) var $name:$ct = (($name:$ct) -> $i{name})(${param});
							}];

							if (variant.params.length > 0) {
								Context.error('Unexpected amount of variant parameters.', variant.params[0].pos);
							}

							switch (f.expr.expr) {
								case EBlock(b):
									var ff = {
										ret: f.ret,
										params: f.params,
										expr: {pos: variant.pos, expr: EBlock(inits.concat(b))},
										args: [{name: "async", type: macro:utest.Async}]
									};

									newFields.push({
										pos: variant.pos,
										name: field.name + name,
										meta: field.meta.filter(m -> m.name != ":variant"),
										kind: FFun(ff),
										doc: field.doc,
										access : field.access
									});

								case _:
							}
						}
					}
				case _:
			}
		}

		for (f in removedFields) fields.remove(f);
		return fields.concat(newFields);
	}

	static function makeAsyncTest(field:Field) {
		field.meta.push({name: ":coroutine", params: [], pos: field.pos});
		field.meta.push({name: ":timeout", params: [macro 20000], pos: field.pos});
	}
}
