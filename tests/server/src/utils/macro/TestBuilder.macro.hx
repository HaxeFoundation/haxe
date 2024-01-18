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
						makeAsyncTest(f, field.pos);
					} else {
						// TODO: support functions that define their own async arg (not named `_` or `async`)
						var args = f.args.copy();
						f.args = [];
						makeAsyncTest(f, field.pos);

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

	static function makeAsyncTest(f:Function, fpos:Position) {
		var asyncName = switch f.args {
			case []:
				var name = "async";
				f.args.push({
					name: name,
					type: macro:utest.Async
				});
				name;
			case [arg]:
				if (arg.name == "_") {
					arg.name = "async";
					arg.type = macro:utest.Async;
				}
				arg.name;
			case _:
				Context.fatalError('Unexpected amount of test arguments', fpos);
				"";
		}
		switch (f.expr.expr) {
			case EBlock(el):
				var posInfos = Context.getPosInfos(f.expr.pos);
				var pos = Context.makePosition({min: posInfos.max, max: posInfos.max, file: posInfos.file});
				el.push(macro @:pos(pos) {
					if ($i{asyncName}.timedOut) Assert.fail("timeout");
					else $i{asyncName}.done();
				});
				f.expr = macro {
					$i{asyncName}.setTimeout(20000);
					${transformHaxeCalls(asyncName, el)};
				}
			case _:
				Context.error("Block expression expected", f.expr.pos);
		}
	}

	static function transformHaxeCalls(asyncName:String, el:Array<Expr>) {
		var e0 = el.shift();
		if (el.length == 0) {
			return e0;
		} else {
			var e = switch e0 {
				case macro runHaxe($a{args}):
					var e = transformHaxeCalls(asyncName, el);
					args.push(macro() -> ${failOnException(asyncName, e)});
					macro runHaxe($a{args});
				case macro runHaxeJson($a{args}):
					var e = transformHaxeCalls(asyncName, el);
					args.push(macro() -> ${failOnException(asyncName, e)});
					macro runHaxeJson($a{args});
				case macro runHaxeJsonCb($a{args}):
					var e = transformHaxeCalls(asyncName, el);
					args.push(macro() -> ${failOnException(asyncName, e)});
					macro runHaxeJsonCb($a{args});
				case macro complete($a{args}):
					var e = transformHaxeCalls(asyncName, el);
					args.push(macro function(response, markers) ${failOnException(asyncName, e)});
					macro complete($a{args});
				case _:
					macro {$e0; ${transformHaxeCalls(asyncName, el)}};
			}
			e.pos = e0.pos;
			return e;
		}
	}

	static function failOnException(asyncName:String, e:Expr):Expr {
		return macro
			@:pos(e.pos) try {
				$e;
			} catch (e) {
				Assert.fail(e.details());
				$i{asyncName}.done();
				return;
			}
	}
}
