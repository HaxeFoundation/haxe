package haxe.coro;

import haxe.coro.EventLoop;
import haxe.coro.schedulers.EventLoopScheduler;
import haxe.coro.continuations.RacingContinuation;
import haxe.coro.continuations.BlockingContinuation;

private class CoroSuspend<T> extends haxe.coro.BaseContinuation<T> {
	public function new(completion:haxe.coro.IContinuation<T>) {
		super(completion, 1);
	}

	public function invokeResume():SuspensionResult<T> {
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
	public static function suspend<T>(func:haxe.coro.IContinuation<T>->Void, completion:haxe.coro.IContinuation<T>):T {
		var continuation = new CoroSuspend(completion);
		var safe = new haxe.coro.continuations.RacingContinuation(completion, continuation);
		func(safe);
		safe.resolve();
		return cast continuation;
	}

	@:coroutine @:coroutine.nothrow public static function delay(ms:Int):Void {
		Coroutine.suspend(cont -> {
			cont.context.scheduler.scheduleIn(() -> cont.resume(null, null), ms);
		});
	}

	@:coroutine @:coroutine.nothrow public static function yield():Void {
		Coroutine.suspend(cont -> {
			cont.context.scheduler.schedule(() -> cont.resume(null, null));
		});
	}

	public static function run<T>(f:Coroutine<() -> T>):T {
		final loop = new EventLoop();
		final cont = new BlockingContinuation<T>(loop, new EventLoopScheduler(loop));
		final result = f(cont);

		return switch (result.state) {
			case Pending:
				cont.wait();
			case Returned:
				result.result;
			case Thrown:
				throw result.error;
		}
	}
}
