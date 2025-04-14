package haxe.coro.continuations;

import sys.thread.Mutex;

class RacingContinuation<T> implements IContinuation<T> {
	final _hx_completion:IContinuation<Any>;

	final lock:Mutex;

	var assigned:Bool;

	var _hx_result:Any;

	var _hx_error:Any;

	public var _hx_recursing:Bool;

	public final _hx_context:CoroutineContext;

	public function new(completion) {
		_hx_completion = completion;
		_hx_context = _hx_completion._hx_context;
		_hx_result = null;
		_hx_error = null;
		assigned = false;
		lock = new Mutex();
	}

	public function resume(result:T, error:Exception) {
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
