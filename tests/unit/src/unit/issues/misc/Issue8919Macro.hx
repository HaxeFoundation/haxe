package unit.issues.misc;

import haxe.macro.Expr;
import haxe.macro.Context;

using haxe.macro.Tools;

class Issue8919Macro {
	macro static public function check(fieldAccess:Expr):Expr {
		var typed = Context.typeExpr(fieldAccess);
		return switch typed.expr {
			case TField({ t:TType(_.get() => t, []) }, fa) if(t.name == 'TestTd'):
				macro @:pos(fieldAccess.pos) noAssert();
			case _:
				var msg = '`${fieldAccess.toString()}` should be typed as `TField({ t:TType(TypeTd) }, _)`';
				macro @:pos(fieldAccess.pos) assert($v{msg});
		}
	}
}