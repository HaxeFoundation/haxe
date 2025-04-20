package haxe.coro;

import haxe.coro.EventLoop;
import haxe.coro.schedulers.EventLoopScheduler;
import haxe.coro.continuations.RacingContinuation;
import haxe.coro.continuations.BlockingContinuation;

private class CoroSuspend extends haxe.coro.BaseContinuation {
	public function new(completion:haxe.coro.IContinuation<Any>) {
		super(completion, 1);
	}

	public function invokeResume() {
		return Coroutine.suspend(null, this);
	}
}

/**
	Coroutine function.
**/
@:callable
@:coreType
abstract Coroutine<T:haxe.Constraints.Function> {
	@:coroutine @:coroutine.transformed
	public static function suspend<T>(func:haxe.coro.IContinuation<Any>->Void, _hx_completion:haxe.coro.IContinuation<Any>):T {
		var _hx_continuation = new CoroSuspend(_hx_completion);
		var safe = new haxe.coro.continuations.RacingContinuation(_hx_completion, _hx_continuation);
		func(safe);
		safe.resolve();
		return cast _hx_continuation;
	}

	@:coroutine public static function delay(ms:Int):Void {
		Coroutine.suspend(cont -> {
			cont._hx_context.scheduler.scheduleIn(() -> cont.resume(null, null), ms);
		});
	}

	@:coroutine public static function yield():Void {
		Coroutine.suspend(cont -> {
			cont._hx_context.scheduler.schedule(() -> cont.resume(null, null));
		});
	}

	public static function run<T>(f:Coroutine<() -> T>):T {
		final loop = new EventLoop();
		final cont = new BlockingContinuation(loop, new EventLoopScheduler(loop));
		final result = f(cont);

		return switch (result._hx_control) {
			case Pending:
				cast cont.wait();
			case Returned:
				cast result._hx_result;
			case Thrown:
				throw result._hx_error;
		}
	}
}
