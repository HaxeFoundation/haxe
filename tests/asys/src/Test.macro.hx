import haxe.macro.Expr;
import haxe.macro.Context;

using haxe.macro.ExprTools;

class Test extends utest.Test {

	macro function allAsync(eThis:Expr, asyncVar:ExprOf<utest.Async>, cpsCalls:Array<Expr>):ExprOf<Void> {
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
		function injectAsyncDoneIntoContinuation(e:Expr) {
			switch e.expr {
				case EBlock(exprs) if(exprs.length > 0):
					injectAsyncDoneIntoContinuation(exprs[exprs.length - 1]);
				case ECall(_,args) if(args.length > 0):
					switch args[args.length - 1].expr {
						case EFunction(_, fn) if(fn.expr != null):
							fn.expr = macro @:pos(e.pos) {
								${injectAsyncDoneBeforeReturns(fn.expr)};
								// ${fn.expr};
								__async__.done();
							}
						case _:
							error(e.pos);
					}
				case _:
					error(e.pos);
			}
			return e;
		}
		var exprs = cpsCalls.map(e -> macro @:pos(e.pos) $asyncVar.branch(__async__ -> ${injectAsyncDoneIntoContinuation(e)}));
		var pos = Context.currentPos();
		return macro @:pos(pos) $b{exprs};
	}
}