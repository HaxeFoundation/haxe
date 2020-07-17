import haxe.macro.Expr;
import haxe.macro.Context;

using haxe.macro.ExprTools;

class Test extends utest.Test {

	macro function asyncAll(eThis:Expr, asyncVar:ExprOf<utest.Async>, cpsCalls:Array<Expr>):ExprOf<Void> {
		if(#if display true || #end Context.defined('display')) {
			return macro $b{cpsCalls};
		}
		function error(pos) {
			Context.error('This expression is not a CPS-call or the last argument of a call is not an anonymous function', pos);
		}
		function injectAsyncDoneBeforeReturns(e:Expr) {
			return switch e.expr {
				case EMeta(m, macro return $e1) if(m.name == ':implicitReturn'):
					e1 = macro return ${injectAsyncDoneBeforeReturns(e1)};
					{ expr:EMeta(m, e1), pos:e.pos };
				case EReturn(null):
					macro @:pos(e.pos) {
						__async__.done();
						return;
					}
				case EReturn(e):
					macro @:pos(e.pos) {
						__async__.done();
						return $e;
					}
				case _:
					e.map(injectAsyncDoneBeforeReturns);
			}
		}
		function injectAsyncDone(e:Expr, requireContinuation:Bool):Expr {
			switch e.expr {
				case ECall(_,args) if(args.length > 0):
					switch args[args.length - 1].expr {
						case EFunction(_, fn) if(fn.expr != null):
							fn.expr = switch fn.expr.expr {
								case EMeta(m, macro return $e1) if(m.name == ':implicitReturn'):
									e1 = injectAsyncDone(injectAsyncDoneBeforeReturns(e1), false);
									macro @:pos(fn.expr.pos) @:implicitReturn return $e1;
								case _:
									injectAsyncDone(injectAsyncDoneBeforeReturns(fn.expr), false);
							}
							return e;
						case _:
							if(requireContinuation) {
								error(e.pos);
							}
							return macro @:pos(e.pos) {
								$e;
								__async__.done();
							}
					}
				case EBlock(exprs) if(exprs.length > 0):
					exprs[exprs.length - 1] = injectAsyncDone(exprs[exprs.length - 1], requireContinuation);
					return e;
				case EIf(econd, eif, eelse):
					eif = injectAsyncDone(eif, requireContinuation);
					if(eelse == null) {
						eelse = macro @:pos(e.pos) __async__.done();
					} else {
						eelse = injectAsyncDone(eelse, requireContinuation);
					}
					e.expr = EIf(econd, eif, eelse);
					return e;
				case ETry(etry, catches):
					etry = injectAsyncDone(etry, requireContinuation);
					for (c in catches) {
						c.expr = injectAsyncDone(c.expr, requireContinuation);
					}
					e.expr = ETry(etry, catches);
					return e;
				case ESwitch(etarget, cases, edef):
					for(c in cases) {
						if(c.expr == null) {
							c.expr = macro @:pos(c.values[0].pos) __async__.done();
						} else {
							c.expr = injectAsyncDone(c.expr, requireContinuation);
						}
					}
					if(edef == null) {
						edef = macro @:pos(e.pos) __async__.done();
					} else {
						edef = injectAsyncDone(edef, requireContinuation);
					}
					e.expr = ESwitch(etarget, cases, edef);
					return e;
				case _:
					if(requireContinuation) {
						error(e.pos);
					}
					return macro @:pos(e.pos) {
						$e;
						__async__.done();
					}
			}
		}
		var exprs = cpsCalls.map(e -> macro @:pos(e.pos) $asyncVar.branch(__async__ -> ${injectAsyncDone(e, true)}));
		var pos = Context.currentPos();
		return macro @:pos(pos) $b{exprs};
	}
}