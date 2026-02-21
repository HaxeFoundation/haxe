package unit;

import haxe.Exception;
import haxe.coro.IContinuation;
import haxe.coro.SuspensionResult;
import haxe.coro.context.Context;
import utest.Assert;

private class AlwaysSuspending {
	public static var _stored:Null<IContinuation<Int>> = null;

	@:coroutine(transformed)
	public static function suspend(cont:IContinuation<Int>):SuspensionResult<Int> {
		_stored = cont;
		return new SuspensionResult<Int>(Pending);
	}
}

private class NothrowCoroutines {
	static function thrower():Void {
		throw "nothrow error";
	}

	// Without nothrow: exception is caught by the coroutine wrapper and forwarded
	// to the continuation via resume(null, error).

	@:coroutine
	public static function withThrow():Void {
		thrower();
	}

	// With nothrow: the outer try/catch wrapper is omitted, so the exception
	// escapes the coroutine call normally.

	@:coroutine(nothrow)
	public static function withNothrow():Void {
		thrower();
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

	// Regression test: when outer is called via BaseContinuation.invokeResume (the recycled-
	// continuation path) and its last action is a tail-call suspension that returns a non-
	// singleton Pending result, mk_suspending_tail_call must normalise the result to the
	// SuspensionResult.suspended singleton.  Without that, BaseContinuation.resume uses
	// reference equality and sees resumeResult != suspended, fires dispatch, hits the Pending
	// branch in onDispatch, and calls completion.resume(null, "Invalid dispatch call ...").
	//
	// To trigger the recycling path we need two sequential suspension calls in outer:
	//   1. A non-tail call (passes _hx_continuation to suspend, which stores it).
	//   2. A tail call (RTailReturn) that is reached only after manually resuming (1).
	function testTailCallReturnPending() {
		AlwaysSuspending._stored = null;

		@:coroutine function outer():Int {
			AlwaysSuspending.suspend(); // non-tail: mk_suspending_call stores outer's BaseContinuation
			return AlwaysSuspending.suspend(); // RTailReturn: mk_suspending_tail_call path
		}

		var cont = new TrackingCont<Int>();
		invokeCoroutine(cont, outer);
		// outer is now suspended at the first suspend() call.
		// AlwaysSuspending._stored is outer's BaseContinuation (bc_outer).

		final bc = AlwaysSuspending._stored;
		AlwaysSuspending._stored = null;
		// Resume bc_outer.  invokeResume() re-enters outer(bc_outer) (recycled continuation)
		// and advances to the RTailReturn suspend call.
		// Without the state-switch fix: outer(bc_outer) returns a non-singleton Pending object
		// to invokeResume(), which then fires BaseContinuation dispatch and sets cont.lastError.
		if (bc != null)
			bc.resume(0, null);

		eq(null, cont.lastError);
	}

	// Tests that @:coroutine(nothrow) omits the outer try/catch, causing exceptions
	// to escape normally instead of being forwarded to the continuation.
	function testCoroutineNothrow() {
		// Without nothrow: exception is caught and forwarded via cont.resume(null, error).
		var cont = new TrackingCont<haxe.Unit>();
		invokeCoroutineVoid(cont, NothrowCoroutines.withThrow);
		eq(1, cont.resumeCount);
		f(cont.lastError == null);

		// With nothrow: exception escapes the coroutine call site.
		Assert.raises(() -> {
			NothrowCoroutines.withNothrow(new SimpleCont());
		}, String);
	}

	// Tests that @:coroutine(nothrow) also works on local functions.
	function testCoroutineNothrowLocal() {
		function thrower():Void {
			throw "nothrow error";
		}

		// Without nothrow: exception is caught and forwarded via cont.resume(null, error).
		@:coroutine function withThrow():Void {
			thrower();
		}
		var cont = new TrackingCont<haxe.Unit>();
		invokeCoroutineVoid(cont, withThrow);
		eq(1, cont.resumeCount);
		f(cont.lastError == null);

		// With nothrow: exception escapes the coroutine call site.
		@:coroutine(nothrow) function withNothrow():Void {
			thrower();
		}
		Assert.raises(() -> {
			withNothrow(new SimpleCont());
		}, String);
	}
}
