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
		Setup test environment for filesystem-related tests.
	**/
	function setupFileSystem() {
		var tempDir = 'test-data/temp';
		//TODO: Find a way to create & cleanup `test-data/temp` directory without using old sys API
		if(!sys.FileSystem.exists(tempDir))
			sys.FileSystem.createDirectory(tempDir);
		switch sys.FileSystem.readDirectory(tempDir) {
			case []:
			case _:
				if(isWindows)
					Sys.command('rmdir', [tempDir, '/S', '/Q'])
				else
					Sys.command('rm', ['-rf', tempDir]);
				sys.FileSystem.createDirectory(tempDir);
		}
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
		Assert `v` is of type `type`. Executes `callback(v)` If assertion holds.
	**/
	inline function assertType<T:Exception>(v:Any, type:Class<T>, callback:(v:T)->Void, ?pos:PosInfos):Void {
		if(isOfType(v, type, pos))
			callback(v);
	}

	/**
		Takes a list of expressions with Continuation-Passing-Style calls like these:
		```haxe
		asyncAll(asyncVar,
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
	macro function asyncAll(eThis:Expr, asyncVar:ExprOf<utest.Async>, cpsCalls:Array<Expr>):ExprOf<Void>;
}