package haxe.coro.continuations;

@:coreApi class RacingContinuation<T> implements IContinuation<T> {
	final _hx_completion:IContinuation<Any>;

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
	}

	public function resume(result:T, error:Exception):Void {
		_hx_context.scheduler.schedule(() -> {
			if (assigned) {
				_hx_completion.resume(result, error);
			} else {
				assigned = true;
				_hx_result = result;
				_hx_error = error;
			}
		});
	}

	public function getOrThrow():Any {
		if (assigned) {
			if (_hx_error != null) {
				throw _hx_error;
			}

			return _hx_result;
		}

		assigned = true;

		return haxe.coro.Primitive.suspended;
	}
}
