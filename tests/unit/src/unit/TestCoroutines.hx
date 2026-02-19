package unit;

import haxe.Exception;
import haxe.coro.IContinuation;
import haxe.coro.SuspensionResult;
import haxe.coro.context.Context;

private class SimpleCont<T> implements IContinuation<T> {
	public var context(get, never):Context;

	public function new() {}

	public function get_context():Context
		return Context.empty;

	public function resume(result:Null<T>, error:Null<Exception>):Void {}
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
}
