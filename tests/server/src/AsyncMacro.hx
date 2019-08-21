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
							el.push(macro @:pos(f.expr.pos) async.done());
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
	}
}
