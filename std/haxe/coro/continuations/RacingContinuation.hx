package haxe.coro.continuations;

#if (target.threaded && !cppia)
import sys.thread.Lock;
import sys.thread.Mutex;
import sys.thread.Thread;
#else
private class Lock {
	public function new() {}

	public inline function release() {}

	public inline function wait(?t:Float) {}
}

private class Mutex {
	public function new() {}

	public inline function acquire() {}

	public inline function release() {}
}

private class Thread {
	public static function create(f:Void->Void) {
		f();
	}
}
#end

@:coreApi class RacingContinuation<T> implements IContinuation<T> {
	final _hx_completion:IContinuation<Any>;

	final lock:Mutex;

	var assigned:Bool;

	var _hx_result:Any;

	var _hx_error:Any;

	public final _hx_context:CoroutineContext;

	public function new(completion:IContinuation<Any>) {
		_hx_completion = completion;
		_hx_context = _hx_completion._hx_context;
		_hx_result = null;
		_hx_error = null;
		assigned = false;
		lock = new Mutex();
	}

	public function resume(result:T, error:Exception):Void {
		_hx_context.scheduler.schedule(() -> {
			lock.acquire();

			if (assigned) {
				lock.release();

				_hx_completion.resume(result, error);
			} else {
				assigned = true;
				_hx_result = result;
				_hx_error = error;

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
