import haxe.macro.Expr;

/**
	Base class for asys tests
**/
class Test extends utest.Test {
	static final __systemName = Sys.systemName();

	var isWindows(get,never):Bool;
	function get_isWindows():Bool {
		return __systemName == 'Windows';
	}

	/**
		Takes a list of expressions with Continuation-Passing-Style calls like these:
		```haxe
		allAsync(asyncVar,
			cpsCall1(arg1, arg2, (error, result) -> {
				doSomething();
			}),
			{
				job();
				cpsCall2(arg1, (error, result) -> {
					doAnotherThing();
				});
			}
		)
		```
		and injects `asyncVar.done()` expressions into continuations:
		```haxe
		{
			asyncVar.branch(__async__ -> cpsCall1(arg1, arg2, (error, result) -> {
				doSomething();
				__async__.done();
			}));
			async.branch(__async__ -> {
				job();
				cpsCall2(arg1, (error, result) -> {
					doAnotherThing();
					__async__.done();
				});
			});
		}
		```
	**/
	macro function allAsync(eThis:Expr, asyncVar:ExprOf<utest.Async>, cpsCalls:Array<Expr>):ExprOf<Void>;
}