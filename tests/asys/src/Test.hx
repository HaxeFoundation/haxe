import haxe.PosInfos;
import haxe.macro.Expr;
import haxe.Exception;

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
		Asserts `e` is `null`.
		Otherwise fails with the message of `e.message`.
	**/
	function noException(e:Null<Exception>, ?pos:PosInfos):Bool {
		return if(e == null) {
			//utest.Assert.isNull is to register the check in utest's report
			isNull(e, pos);
		} else {
			fail(e.message, pos);
		}
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
			asyncVar.branch(__async__ -> {
				job();
				cpsCall2(arg1, (error, result) -> {
					doAnotherThing();
					__async__.done();
				});
			});
		}
		```
		INFO: does not inject `async.done()` calls into loops.
	**/
	macro function allAsync(eThis:Expr, asyncVar:ExprOf<utest.Async>, cpsCalls:Array<Expr>):ExprOf<Void>;
}