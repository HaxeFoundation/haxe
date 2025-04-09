package haxe.coro;

import sys.thread.Mutex;
import sys.thread.EventLoop;
import haxe.coro.schedulers.EventLoopScheduler;
import haxe.coro.continuations.BlockingContinuation;

private class RacingContinuation<T> implements IContinuation<T> {
    final _hx_completion:IContinuation<Any>;
    
    final lock:Mutex;

    var assigned:Bool;

    var _hx_result:Any;

    var _hx_error:Any;

	public final _hx_context:CoroutineContext;

    public function new(completion) {
        _hx_completion = completion;
        _hx_context    = _hx_completion._hx_context;
        _hx_result     = null;
        _hx_error      = null;
        assigned       = false;
        lock           = new Mutex();
    }

    public function resume(result:T, error:Exception) {
        _hx_context.scheduler.schedule(() -> {
            lock.acquire();

            if (assigned) {
                lock.release();
    
                _hx_completion.resume(result, error);
            } else {
                assigned   = true;
                _hx_result = result;
                _hx_error  = error;

                lock.release();
            }
        });
    }

    public function getOrThrow():Any {
        lock.acquire();

        if (assigned) {
            if (_hx_error != null) {
                final tmp = _hx_error;

                lock.release();

                throw tmp;
            }

            final tmp = _hx_result;

            lock.release();

            return tmp;
        }

        assigned = true;

        lock.release();

        return haxe.coro.Primitive.suspended;
    }
}

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
		final loop = new EventLoop();
        final cont = new BlockingContinuation(loop, new EventLoopScheduler(loop));

		f(cont);

		return cast cont.wait();
	}
}
