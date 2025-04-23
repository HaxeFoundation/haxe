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
	final inputCont:IContinuation<T>;
	final outputCont:SuspensionResult<T>;

	final lock:Mutex;

	var assigned:Bool;

	public final _hx_context:CoroutineContext;

	public function new(inputCont:IContinuation<T>, outputCont:SuspensionResult<T>) {
		this.inputCont = inputCont;
		this.outputCont = outputCont;
		_hx_context = inputCont._hx_context;
		assigned = false;
		lock = new Mutex();
	}

	public function resume(result:T, error:Exception):Void {
		_hx_context.scheduler.schedule(() -> {
			lock.acquire();

			if (assigned) {
				lock.release();
				inputCont.resume(result, error);
			} else {
				assigned = true;
				outputCont._hx_result = result;
				outputCont._hx_error = error;

				lock.release();
			}
		});
	}

	public function resolve():Void {
		lock.acquire();
		if (assigned) {
			if (outputCont._hx_error != null) {
				outputCont._hx_control = Thrown;
				lock.release();
			} else {
				outputCont._hx_control = Returned;
				lock.release();
			}
		} else {
			assigned = true;
			outputCont._hx_control = Pending;
			lock.release();
		}
	}
}
