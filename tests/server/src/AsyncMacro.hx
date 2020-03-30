import haxe.macro.Expr;
import haxe.macro.Context;

using StringTools;

class AsyncMacro {
	static public macro function build():Array<Field> {
		var fields = Context.getBuildFields();
		for (field in fields) {
			if (!field.name.startsWith("test")) {
				continue;
			}
			switch (field.kind) {
				case FFun(f):
					f.args.push({
						name: "async",
						type: macro:utest.Async
					});
					switch (f.expr.expr) {
						case EBlock(el):
							var posInfos = Context.getPosInfos(f.expr.pos);
							var pos = Context.makePosition({min: posInfos.max, max: posInfos.max, file: posInfos.file});
							el.push(macro @:pos(pos) async.done());
							f.expr = transformHaxeCalls(el);
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
		return if (el.length == 0) {
			e0;
		} else switch (e0) {
			case macro runHaxe($a{args}):
				var e = transformHaxeCalls(el);
				args.push(macro() -> $e);
				macro @:pos(e0.pos) runHaxe($a{args});
			case macro runHaxeJson($a{args}):
				var e = transformHaxeCalls(el);
				args.push(macro() -> $e);
				macro @:pos(e0.pos) runHaxeJson($a{args});
			case macro complete($a{args}):
				var e = transformHaxeCalls(el);
				args.push(macro function(response, markers) $e);
				macro @:pos(e0.pos) complete($a{args});
			case _:
				macro {$e0; ${transformHaxeCalls(el)}};
		}
	}
}
