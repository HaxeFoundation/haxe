package unit;

import haxe.Exception;
import haxe.coro.IContinuation;
import haxe.coro.SuspensionResult;
import haxe.coro.context.Context;

// A manually-transformed coroutine that always suspends (returns non-singleton Pending).
// Used to simulate what Coro.suspend does, without requiring the hxcoro library.
private class AlwaysSuspending {
	@:coroutine @:coroutine.transformed
	public static function suspend(cont:IContinuation<Int>):SuspensionResult<Int> {
		return new SuspensionResult<Int>(Pending);
	}
}

private class SimpleCont<T> implements IContinuation<T> {
	public var context(get, never):Context;

	public function new() {}

	public function get_context():Context
		return Context.empty;

	public function resume(result:Null<T>, error:Null<Exception>):Void {}
}

private class TrackingCont<T> implements IContinuation<T> {
	public var context(get, never):Context;
	public var resumeCount = 0;
	public var lastResult:Null<T> = null;
	public var lastError:Null<Exception> = null;

	public function new() {}

	public function get_context():Context
		return Context.empty;

	public function resume(result:Null<T>, error:Null<Exception>):Void {
		resumeCount++;
		lastResult = result;
		lastError = error;
	}
}

function invokeCoroutine<T>(cont:IContinuation<T>, f:haxe.coro.Coroutine<() -> T>) {
	final result:SuspensionResult<T> = f(cont);
	switch (result.state) {
		case Pending:
		case Returned:
			cont.resume(result.result, null);
		case Thrown:
			cont.resume(null, result.error);
	}
}

function invokeCoroutineVoid(cont:IContinuation<haxe.Unit>, f:haxe.coro.Coroutine<() -> Void>) {
	final result:SuspensionResult<haxe.Unit> = f(cont);
	switch (result.state) {
		case Pending:
		case Returned:
			cont.resume(result.result, null);
		case Thrown:
			cont.resume(null, result.error);
	}
}

class TestCoroutines extends Test {
	// Tests that ||/&& with @:coroutine operands correctly short-circuit.
	function testShortCircuit() {
		var callCount = 0;

		@:coroutine function boolRet(v:Bool):Bool {
			callCount++;
			return v;
		}

		var cont = new SimpleCont<Bool>();

		// || short-circuit: true || ? should not invoke right operand
		callCount = 0;
		(@:coroutine function():Bool return boolRet(true) || boolRet(false))(cont);
		eq(1, callCount);

		// || no short-circuit: false || ? invokes right operand
		callCount = 0;
		(@:coroutine function():Bool return boolRet(false) || boolRet(true))(cont);
		eq(2, callCount);

		// && short-circuit: false && ? should not invoke right operand
		callCount = 0;
		(@:coroutine function():Bool return boolRet(false) && boolRet(true))(cont);
		eq(1, callCount);

		// && no short-circuit: true && ? invokes right operand
		callCount = 0;
		(@:coroutine function():Bool return boolRet(true) && boolRet(false))(cont);
		eq(2, callCount);
	}

	// Tests that a void coroutine tail-calling another void coroutine completes correctly.
	function testTailCallVoid() {
		var called = false;

		@:coroutine function inner() {
			called = true;
		}

		@:coroutine function outer() {
			inner();
		}

		var cont = new TrackingCont<haxe.Unit>();
		invokeCoroutineVoid(cont, outer);
		t(called);
		eq(1, cont.resumeCount);
		eq(null, cont.lastError);
	}

	// Tests that a coroutine tail-calling another coroutine propagates the return value.
	function testTailCallReturn() {
		@:coroutine function inner():Int {
			return 42;
		}

		@:coroutine function outer():Int {
			return inner();
		}

		var cont = new TrackingCont<Int>();
		invokeCoroutine(cont, outer);
		eq(1, cont.resumeCount);
		eq(42, cont.lastResult);
		eq(null, cont.lastError);
	}

	// Tests that a tail call with arguments works correctly.
	function testTailCallWithArgs() {
		@:coroutine function add(a:Int, b:Int):Int {
			return a + b;
		}

		@:coroutine function compute():Int {
			return add(10, 32);
		}

		var cont = new TrackingCont<Int>();
		invokeCoroutine(cont, compute);
		eq(1, cont.resumeCount);
		eq(42, cont.lastResult);
		eq(null, cont.lastError);
	}

	// Regression test: when a coroutine in RTailReturn position returns a Pending
	// SuspensionResult (not the SuspensionResult.suspended singleton), the TCO path
	// must normalise it to the singleton. Otherwise BaseContinuation.resume, which
	// uses reference equality against the singleton to suppress dispatch, would fire
	// BaseContinuation.onDispatch with a Pending result and produce
	// "Invalid dispatch call on suspended coroutine".
	//
	// AlwaysSuspending.suspend() simulates what hxcoro's Coro.suspend does: it is
	// a @:coroutine.transformed function that returns a freshly-constructed (non-
	// singleton) SuspensionResult in Pending state.
	function testTailCallReturnPending() {
		@:coroutine function outer():Int {
			return AlwaysSuspending.suspend(); // RTailReturn
		}

		var cont = new TrackingCont<Int>();
		final result = outer(cont);
		// With the fix the generated code is:
		//   let _hx_tmp = AlwaysSuspending.suspend(_hx_continuation.completion);
		//   switch(_hx_tmp.state) { case 0: return SuspensionResult.suspended; default: return _hx_tmp; }
		// so result must be the singleton.
		// Without the fix the code was just:
		//   return AlwaysSuspending.suspend(_hx_continuation.completion);
		// which returns the non-singleton Pending object, causing dispatch errors.
		t(result == SuspensionResult.suspended);
		eq(0, cont.resumeCount);
	}
}
