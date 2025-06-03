package haxe.coro.continuations;

import haxe.coro.context.Context;
import haxe.coro.schedulers.Scheduler;

class RacingContinuation<T> extends SuspensionResult<T> implements IContinuation<T> {
	final inputCont:IContinuation<T>;

	var mutex:Null<Mutex>;

	public var context(get, never):Context;

	final scheduler:Scheduler;

	public function new(inputCont:IContinuation<T>) {
		this.inputCont = inputCont;
		mutex = new Mutex();
		scheduler = context.get(Scheduler.key);
	}

	inline function get_context() {
		return inputCont.context;
	}

	public function resume(result:T, error:Exception):Void {
		// store in a local to avoid `this` capturing.
		final inputCont = inputCont;
		inline function resumeContinue(result:T, error:Exception) {
			scheduler.schedule(0, () -> {
				inputCont.resume(result, error);
			});
		}

		// Store mutex as stack value.
		final mutex = mutex;
		if (mutex == null) {
			// If that's already null we're definitely done.
			return resumeContinue(result, error);
		}
		// Otherwise we take the mutex now. We know that the stack value isn't null, so that's safe.
		mutex.acquire();
		if (this.mutex == null) {
			// The shared reference has become null in the meantime, so we're done.
			mutex.release();
			return resumeContinue(result, error);
		}
		// At this point we own the mutex, so we're first. We can set the shared reference to null and release it.
		this.mutex = null;
		mutex.release();
		this.result = result;
		this.error = error;
	}


	public function resolve():Void {
		// same logic as resume
		final mutex = mutex;
		if (mutex == null) {
			if (error != null) {
				state = Thrown;
			} else {
				state = Returned;
			}
			return;
		}
		mutex.acquire();
		if (this.mutex == null) {
			mutex.release();
			if (error != null) {
				state = Thrown;
			} else {
				state = Returned;
			}
			return;
		}
		this.mutex = null;
		mutex.release();
		state = Pending;
	}
}
