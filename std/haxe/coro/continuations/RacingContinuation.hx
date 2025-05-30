package haxe.coro.continuations;

import haxe.coro.context.Context;
import haxe.coro.schedulers.Scheduler;

@:coreApi class RacingContinuation<T> extends SuspensionResult<T> implements IContinuation<T> {
	final inputCont:IContinuation<T>;

	final lock:Mutex;

	var assigned:Bool;

	public var context(get, null):Context;

	public function new(inputCont:IContinuation<T>) {
		this.inputCont = inputCont;
		context = inputCont.context;
		assigned = false;
		lock = new Mutex();
	}

	inline function get_context() {
		return context;
	}

	public function resume(result:T, error:Exception):Void {
		context.get(Scheduler.key).schedule(0, () -> {
			lock.acquire();

			if (assigned) {
				lock.release();
				inputCont.resume(result, error);
			} else {
				assigned = true;
				this.result = result;
				this.error = error;

				lock.release();
			}
		});
	}

	public function resolve():Void {
		lock.acquire();
		if (assigned) {
			if (error != null) {
				state = Thrown;
				lock.release();
			} else {
				state = Returned;
				lock.release();
			}
		} else {
			assigned = true;
			state = Pending;
			lock.release();
		}
	}
}
