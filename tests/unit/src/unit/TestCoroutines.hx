package unit;

import haxe.Exception;
import haxe.coro.IContinuation;
import haxe.coro.SuspensionResult;
import haxe.coro.context.Context;
import utest.Assert;

// Generic suspension helper: stores the continuation and suspends without auto-resuming.
// The caller is responsible for resuming via `s.cont.resume(...)`.
private class Suspender<T> {
	public var cont:Null<IContinuation<T>> = null;

	public function new() {}

	@:coroutine(transformed)
	public function suspend(cont:IContinuation<T>):SuspensionResult<T> {
		this.cont = cont;
		return new SuspensionResult(Pending);
	}
}

// Regression test for hxcoro#95: overriding coroutines used to endlessly call each
// other because BaseContinuation.invokeResume() dispatched dynamically to the
// original function, which could call the child's override.  With the new design the
// state machine is embedded in a thunk inside each class's own invokeResume, so there
// is no dynamic dispatch back to the original virtual function.
private class ParentCoro {
	public var log:Array<String> = [];

	public function new() {}

	@:coroutine public function test() {
		@:coroutine function id(x:Int):Int {
			return x;
		}
		log.push("parent-" + id(1));
	}
}

private class ChildCoro extends ParentCoro {
	@:coroutine override public function test() {
		@:coroutine function id(x:Int):Int {
			return x;
		}
		log.push("child-" + id(2));
		super.test();
	}
}

private class NothrowCoroutines {
	static function thrower():Void {
		throw "nothrow error";
	}

	// Without outcome.noThrow: exception is caught by the coroutine wrapper and forwarded
	// to the continuation via resume(null, error).

	@:coroutine
	public static function withThrow():Void {
		thrower();
	}

	// With outcome={noThrow}: the outer try/catch wrapper is omitted, so the exception
	// escapes the coroutine call normally.

	@:coroutine(outcome = {noThrow: true})
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

// A simple async iterator that counts from 0 up to (but not including) `limit`.
// If `suspender` is provided, each call to `hasNext` suspends via it.
private class CountingAsyncIterator {
	var i:Int;
	final limit:Int;
	final suspender:Null<Suspender<Bool>>;

	public function new(limit:Int, ?suspender:Suspender<Bool>) {
		i = 0;
		this.limit = limit;
		this.suspender = suspender;
	}

	@:coroutine public function hasNext():Bool {
		if (suspender != null)
			suspender.suspend();
		return i < limit;
	}

	public function next():Int {
		return i++;
	}
}

// An async iterable wrapping a `CountingAsyncIterator`.
private class CountingAsyncIterable {
	final limit:Int;

	public function new(limit:Int) {
		this.limit = limit;
	}

	public function iterator():haxe.coro.AsyncIterator<Int> {
		return new CountingAsyncIterator(limit);
	}
}

// An abstract that has array-access iteration (returning element * 10) but also
// provides an async iterator (returning [0,1,2]).  Used to assert that the async
// iterator takes priority over array access in a coroutine context.
private abstract AsyncIterablePriority(Array<Int>) {
	public inline function new(arr:Array<Int>)
		this = arr;

	public function get_length():Int
		return this.length;

	@:arrayAccess public inline function get(i:Int):Int
		return this[i] * 10;

	public function iterator():haxe.coro.AsyncIterator<Int> {
		return new CountingAsyncIterator(3);
	}
}

function invokeCoroutine<T>(cont:IContinuation<T>, f:haxe.coro.Coroutine<() -> T>) {
	f(cont);
}

function invokeCoroutineVoid(cont:IContinuation<haxe.Unit>, f:haxe.coro.Coroutine<() -> Void>) {
	f(cont);
}

// Helper with outcome={noSuspend,noThrow}: always returns 42 immediately without suspending.
private class NeverReturns42 {
	@:coroutine(transformed, outcome = {noSuspend: true, noThrow: true})
	public static function get(cont:IContinuation<Int>):SuspensionResult<Int> {
		return SuspensionResult.withResult(42);
	}
}

// Helper with outcome={noReturn,noThrow}: always suspends; stores the continuation for manual resume.
private class AlwaysSuspender<T> {
	public var cont:Null<IContinuation<T>> = null;

	public function new() {}

	@:coroutine(transformed, outcome = {noReturn: true, noThrow: true})
	public function suspend(cont:IContinuation<T>):SuspensionResult<T> {
		this.cont = cont;
		return new SuspensionResult(Pending);
	}
}

// Abstract type with a @:coroutine constructor (abstract constructors are transformed
// to static functions early, so they are valid coroutine targets).
private abstract CoroAbstract(Int) {
	@:coroutine public function new(value:Int) {
		this = value;
	}

	public function getValue():Int
		return this;
}

class TestCoroutines extends Test {
	// Tests that @:coroutine is allowed on abstract constructors and works correctly.
	function testAbstractConstructor() {
		var cont = new TrackingCont<CoroAbstract>();
		invokeCoroutine(cont, @:coroutine function():CoroAbstract {
			return new CoroAbstract(42);
		});
		eq(42, cont.lastResult.getValue());
	}

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
		final s = new Suspender<Int>();

		@:coroutine function outer():Int {
			s.suspend(); // non-tail: mk_suspending_call stores outer's BaseContinuation
			return s.suspend(); // RTailReturn: mk_suspending_tail_call path
		}

		var cont = new TrackingCont<Int>();
		invokeCoroutine(cont, outer);
		// outer is now suspended at the first s.suspend() call.
		// s.cont is outer's BaseContinuation (bc_outer).

		final bc = s.cont;
		s.cont = null;
		// Resume bc_outer.  invokeResume() re-enters outer(bc_outer) (recycled continuation)
		// and advances to the RTailReturn suspend call.
		// Without the state-switch fix: outer(bc_outer) returns a non-singleton Pending object
		// to invokeResume(), which then fires BaseContinuation dispatch and sets cont.lastError.
		if (bc != null)
			bc.resume(0, null);

		eq(null, cont.lastError);
	}

	// Tests that @:coroutine(outcome = {noThrow: true}) omits the outer try/catch, causing exceptions
	// to escape normally instead of being forwarded to the continuation.
	function testCoroutineNothrow() {
		// Without outcome.noThrow: exception is caught and forwarded via cont.resume(null, error).
		var cont = new TrackingCont<haxe.Unit>();
		invokeCoroutineVoid(cont, NothrowCoroutines.withThrow);
		eq(1, cont.resumeCount);
		f(cont.lastError == null);

		// With outcome={noThrow}: exception escapes the coroutine call site.
		Assert.raises(() -> {
			NothrowCoroutines.withNothrow(new SimpleCont());
		}, String);
	}

	// Regression test: a coroutine that suspends and then recursively calls itself used to
	// fail with "Invalid coroutine state" because the old instanceof+recursing mechanism
	// incorrectly reused the continuation object on a recursive call made during a resume.
	// With the new invokeResume-based design there is no such check; each call to the
	// original function always allocates a fresh continuation.
	function testRecursiveAfterSuspension() {
		@:coroutine function yield_() {}
		var maxIters = 3;
		var counter = 0;
		@:coroutine function foo() {
			if (++counter < maxIters) {
				yield_();
				foo();
			}
		}
		var cont = new TrackingCont<haxe.Unit>();
		invokeCoroutineVoid(cont, foo);
		eq(null, cont.lastError);
		eq(maxIters, counter);
	}

	// Regression test for the "skip unused _hx_result / skip gotoLabel in single-state"
	// optimisations: a single-state coroutine (no suspension points) that just returns a
	// value must still return the correct result.
	function testSingleStateResultOptimisation() {
		@:coroutine function identity(x:Int):Int {
			return x;
		}

		var cont = new TrackingCont<Int>();
		@:coroutine function wrapper():Int
			return identity(42);
		invokeCoroutine(cont, wrapper);
		eq(null, cont.lastError);
		eq(42, cont.lastResult);
	}

	// Regression test for the "skip unused _hx_result" optimisation in multi-state coros:
	// when the coroutine suspends but does NOT use the result (SusBlock), _hx_result is
	// omitted. Verify that the resumed coroutine still completes correctly.
	function testMultiStateNoResultOptimisation() {
		var resumed = false;

		final sus = new Suspender();

		@:coroutine function waitAndFlag() {
			sus.suspend();
			resumed = true;
		}

		var cont = new TrackingCont<haxe.Unit>();
		waitAndFlag(cont);
		f(resumed); // not yet resumed
		sus.cont.resume(null, null);
		t(resumed); // now resumed
		eq(null, cont.lastError);
	}

	// Regression test for hxcoro#95: overriding a coroutine method used to cause infinite
	// mutual recursion because the old invokeResume() dispatched dynamically back to the
	// overridden method on the child.  With the state machine in a thunk inside each class's
	// own invokeResume(), calling child.test no longer loops.
	// Also covers the super.test() case (hxcoro#95 extension): `super` inside the thunk
	// closure is invalid in most languages, so the compiler inserts a helper method
	// _hx_super_test_0 on ChildCoro that delegates to super.test().
	function testOverridingCoroutine() {
		var child = new ChildCoro();
		var cont = new TrackingCont<haxe.Unit>();
		invokeCoroutineVoid(cont, child.test);
		eq(null, cont.lastError);
		Assert.same(["child-2", "parent-1"], child.log);
	}

	// Tests that @:coroutine(outcome = {noThrow: true}) also works on local functions.
	function testCoroutineNothrowLocal() {
		function thrower():Void {
			throw "nothrow error";
		}

		// Without outcome.noThrow: exception is caught and forwarded via cont.resume(null, error).
		@:coroutine function withThrow():Void {
			thrower();
		}
		var cont = new TrackingCont<haxe.Unit>();
		invokeCoroutineVoid(cont, withThrow);
		eq(1, cont.resumeCount);
		f(cont.lastError == null);

		// With outcome={noThrow}: exception escapes the coroutine call site.
		@:coroutine(outcome = {noThrow: true}) function withNothrow():Void {
			thrower();
		}
		Assert.raises(() -> {
			withNothrow(new SimpleCont());
		}, String);
	}

	// Tests that a for loop over an AsyncIterator works inside a coroutine context.
	function testAsyncIteratorFor() {
		@:coroutine function collectItems():Array<Int> {
			final it = new CountingAsyncIterator(3);
			final ret = [];
			for (v in it) {
				ret.push(v);
			}
			return ret;
		}

		var cont = new TrackingCont<Array<Int>>();
		invokeCoroutine(cont, collectItems);
		eq(1, cont.resumeCount);
		eq(null, cont.lastError);
		Assert.same([0, 1, 2], cont.lastResult);
	}

	// Tests that a for loop over an AsyncIterable works inside a coroutine context.
	function testAsyncIterableFor() {
		@:coroutine function collectItems():Array<Int> {
			final it = new CountingAsyncIterable(3);
			final ret = [];
			for (v in it) {
				ret.push(v);
			}
			return ret;
		}

		var cont = new TrackingCont<Array<Int>>();
		invokeCoroutine(cont, collectItems);
		eq(1, cont.resumeCount);
		eq(null, cont.lastError);
		Assert.same([0, 1, 2], cont.lastResult);
	}

	// Tests that a for loop over a suspending AsyncIterator works correctly.
	// Uses manual pumping: each call to s.cont.resume() advances one hasNext suspension.
	function testAsyncIteratorForSuspend() {
		final s = new Suspender<Bool>();
		final cont = new TrackingCont<Array<Int>>();

		@:coroutine function collectItems():Array<Int> {
			final it = new CountingAsyncIterator(3, s);
			final ret = [];
			for (v in it) {
				ret.push(v);
			}
			return ret;
		}

		invokeCoroutine(cont, collectItems);
		// collectItems is now suspended at the first hasNext call.
		// Pump each suspension manually until collectItems completes.
		while (s.cont != null) {
			final c = s.cont;
			s.cont = null;
			// Resume the suspended hasNext; result is ignored since hasNext re-evaluates
			// `i < limit` itself after resumption.
			c.resume(null, null);
		}

		eq(1, cont.resumeCount);
		eq(null, cont.lastError);
		Assert.same([0, 1, 2], cont.lastResult);
	}

	// Tests that in a coroutine context, AsyncIterator takes priority over array access
	// when a type exposes both.
	function testAsyncIteratorPriority() {
		@:coroutine function collectItems():Array<Int> {
			// AsyncIterablePriority has get_length + @:arrayAccess (returning element * 10)
			// AND iterator():AsyncIterator<Int> (always returning [0,1,2]).
			// In a coroutine context the async iterator must win.
			final arr = new AsyncIterablePriority([1, 2, 3]);
			final ret = [];
			for (v in arr) {
				ret.push(v);
			}
			return ret;
		}

		var cont = new TrackingCont<Array<Int>>();
		invokeCoroutine(cont, collectItems);
		eq(1, cont.resumeCount);
		eq(null, cont.lastError);
		// Async iterator gives [0, 1, 2]; array access would give [10, 20, 30].
		Assert.same([0, 1, 2], cont.lastResult);
	}

	// Regression test for the synchronous manual continuation scenario: when a coroutine
	// completes synchronously (without suspending), the continuation must be called
	// automatically, even when invoked from a non-coroutine context.
	function testSyncManualContinuation() {
		// A coroutine that completes synchronously (no suspension points).
		@:coroutine function syncPath():String {
			return "syncPath value";
		}

		// A coroutine that suspends before returning.
		final sus = new Suspender<String>();
		@:coroutine function asyncPath():String {
			sus.suspend();
			return "asyncPath value";
		}

		// Async case: continuation not called until manually resumed.
		var cont = new TrackingCont<String>();
		asyncPath(cont);
		eq(0, cont.resumeCount); // not yet resumed
		sus.cont.resume(null, null);
		eq(1, cont.resumeCount); // resumed after manual pump
		eq("asyncPath value", cont.lastResult);
		eq(null, cont.lastError);

		// Sync case: continuation must be called immediately.
		cont = new TrackingCont<String>();
		syncPath(cont);
		eq(1, cont.resumeCount); // resumed synchronously
		eq("syncPath value", cont.lastResult);
		eq(null, cont.lastError);
	}

	// Tests that a coroutine declared with suspends=Never works correctly.
	// The Never callee returns immediately; the caller should get the result without
	// creating a real suspension point.
	function testSuspendsNever() {
		// This pre-transformed helper always returns 42 immediately.
		@:coroutine function caller():Int {
			final v = NeverReturns42.get();
			return v + 1;
		}
		var cont = new TrackingCont<Int>();
		invokeCoroutine(cont, caller);
		eq(1, cont.resumeCount);
		eq(43, cont.lastResult);
		eq(null, cont.lastError);
	}

	// Tests that a coroutine declared with suspends=Always suspends the caller.
	function testSuspendsAlways() {
		final sus = new AlwaysSuspender<Int>();
		@:coroutine function caller():Int {
			final v = sus.suspend();
			return v + 1;
		}
		var cont = new TrackingCont<Int>();
		invokeCoroutine(cont, caller);
		// Always-suspending: cont not yet resumed.
		eq(0, cont.resumeCount);
		// Manually resume with result 10.
		sus.cont.resume(10, null);
		eq(1, cont.resumeCount);
		eq(11, cont.lastResult);
		eq(null, cont.lastError);
	}

	// Regression: when a while-condition contains two coroutine calls joined by && or ||,
	// the compiler wraps the condition in TParenthesis which turned `ret` into
	// `RMapExpr(RValue, f)`.  check_complex fell through to `| _ -> e_no_value` so no
	// temp var was allocated and the state machine used `!null` as the branch guard,
	// meaning the loop body was never entered.
	function testWhileConditionMultipleCoroCalls() {
		var items:Array<Int> = [];
		var pos = 0;

		@:coroutine function peek():Int {
			return items[pos];
		}

		@:coroutine function consume():Void {
			pos++;
		}

		// && version: loop while peek() != 0 AND peek() != -1
		@:coroutine function countAnd():Int {
			var count = 0;
			while (peek() != 0 && peek() != -1) {
				consume();
				count++;
			}
			return count;
		}

		var cont = new TrackingCont<Int>();
		items = [1, 2, 3, 0];
		pos = 0;
		invokeCoroutine(cont, countAnd);
		eq(null, cont.lastError);
		eq(3, cont.lastResult);

		// || version: loop while peek() == 5 OR peek() == 99
		// (stops when peek() is neither 5 nor 99, i.e. when it reads 0)
		@:coroutine function countOr():Int {
			var count = 0;
			while (peek() == 5 || peek() == 99) {
				consume();
				count++;
			}
			return count;
		}

		var cont2 = new TrackingCont<Int>();
		items = [5, 5, 5, 0];
		pos = 0;
		invokeCoroutine(cont2, countOr);
		eq(null, cont2.lastError);
		eq(3, cont2.lastResult);
	}
}
