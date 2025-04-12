package haxe.coro;

import sys.thread.Mutex;
import sys.thread.EventLoop;
import haxe.coro.schedulers.EventLoopScheduler;
import haxe.coro.continuations.RacingContinuation;
import haxe.coro.continuations.BlockingContinuation;

/**
	Coroutine function.
**/
@:callable
@:coreType
abstract Coroutine<T:haxe.Constraints.Function> {
	@:coroutine public static function suspend<T>(func:(IContinuation<Any>)->Void):T {
		final cont = haxe.coro.Intrinsics.currentContinuation();
		final safe = new RacingContinuation(cont);

		func(safe);

		// This cast is important, need to figure out why / if there's a better solution.
		return cast safe.getOrThrow();
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

	public static function run<T>(f:Coroutine<()->T>) {
		final loop   = new EventLoop();
		final cont   = new BlockingContinuation(loop, new EventLoopScheduler(loop));
		final result = f(cont);

		return if (result is Primitive) {
			cast cont.wait();
		} else {
			cast result;
		}
	}
}
