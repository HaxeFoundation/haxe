package utils.macro;

import haxe.macro.Expr;
import haxe.macro.Context;

using StringTools;

class TestBuilder {
	static public function build(fields:Array<Field>):Array<Field> {
		for (field in fields) {
			if (!field.name.startsWith("test")) {
				continue;
			}
			switch (field.kind) {
				case FFun(f):
					var asyncName = switch f.args {
						case []:
							var name = "async";
							f.args.push({
								name: name,
								type: macro:utest.Async
							});
							name;
						case [arg]:
							if(arg.name == "_") {
								arg.name = "async";
								arg.type = macro:utest.Async;
							}
							arg.name;
						case _:
							Context.fatalError('Unexpected amount of test arguments', field.pos);
							"";
					}
					switch (f.expr.expr) {
						case EBlock(el):
							var posInfos = Context.getPosInfos(f.expr.pos);
							var pos = Context.makePosition({min: posInfos.max, max: posInfos.max, file: posInfos.file});
							el.push(macro @:pos(pos) $i{asyncName}.done());
							f.expr = macro {
								$i{asyncName}.setTimeout(10000);
								${transformHaxeCalls(el)};
							}
						case _:
							Context.error("Block expression expected", f.expr.pos);
					}
				case _:
			}
		}
		return fields;
	}

	static function transformHaxeCalls(el:Array<Expr>) {
		var e0 = el.shift();
		if (el.length == 0) {
			return e0;
		} else {
			var e = switch e0 {
				case macro runHaxe($a{args}):
					var e = transformHaxeCalls(el);
					args.push(macro() -> $e);
					macro runHaxe($a{args});
				case macro runHaxeJson($a{args}):
					var e = transformHaxeCalls(el);
					args.push(macro() -> $e);
					macro runHaxeJson($a{args});
				case macro complete($a{args}):
					var e = transformHaxeCalls(el);
					args.push(macro function(response, markers) $e);
					macro complete($a{args});
				case _:
					macro {$e0; ${transformHaxeCalls(el)}};
			}
			e.pos = e0.pos;
			return e;
		}
	}
}
