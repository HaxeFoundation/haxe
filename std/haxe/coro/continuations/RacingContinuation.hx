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

	public final context:CoroutineContext;

	public function new(inputCont:IContinuation<T>, outputCont:SuspensionResult<T>) {
		this.inputCont = inputCont;
		this.outputCont = outputCont;
		context = inputCont.context;
		assigned = false;
		lock = new Mutex();
	}

	public function resume(result:T, error:Exception):Void {
		context.scheduler.schedule(() -> {
			lock.acquire();

			if (assigned) {
				lock.release();
				inputCont.resume(result, error);
			} else {
				assigned = true;
				outputCont.result = result;
				outputCont.error = error;

				lock.release();
			}
		});
	}

	public function resolve():Void {
		lock.acquire();
		if (assigned) {
			if (outputCont.error != null) {
				outputCont.control = Thrown;
				lock.release();
			} else {
				outputCont.control = Returned;
				lock.release();
			}
		} else {
			assigned = true;
			outputCont.control = Pending;
			lock.release();
		}
	}
}
