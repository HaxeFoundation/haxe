package unit.issues.misc;

import haxe.macro.Expr;
import haxe.macro.Context;

class Issue9661Macro {
	public macro static function run(e:Expr) {
		switch e {
			case {expr: EBinop(op, e1, e2), pos: pos}:
				switch Context.typeExpr(e) {
					case t_expr = {expr: TBinop(t_op, t_e1, t_e2)}:
						var lstored = Context.storeTypedExpr(t_e1);
						var rstored = Context.storeTypedExpr(t_e2);
						return macro {
							var lh = $lstored;
							var rh = $rstored;
							${{expr: EBinop(op, macro @:pos(e1.pos) lh, macro @:pos(e2.pos) rh), pos: pos}};
						}
					case _:
						throw 'unreachable';
				}
			case _:
					throw 'unreachable';

		}
	}
}