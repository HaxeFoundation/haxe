import haxe.macro.Expr;
import haxe.macro.Context;

class AsyncMacro {
	static public macro function async(e:Expr) {
		var el = switch (e.expr) {
			case EBlock(el): el;
			case _: Context.error("Block expression expected", e.pos);
		}
		el.unshift(macro var _done = utest.Assert.createAsync(5000));
		el.push(macro _done());
		function loop(el:Array<Expr>) {
			var e0 = el.shift();
			return if (el.length == 0) {
				e0;
			} else switch (e0) {
				case macro runHaxe($a{args}):
					var e = loop(el);
					args.push(macro () -> $e);
					macro runHaxe($a{args});
				case _:
					macro { $e0; ${loop(el)}};
			}
		}
		return loop(el);
	}
}