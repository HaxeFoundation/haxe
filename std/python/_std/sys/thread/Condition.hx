package sys.thread;

@:coreApi
class Condition {
	final cond:python.lib.threading.Condition;

	public function new():Void {
		this.cond = new python.lib.threading.Condition();
	}

	public function acquire():Void {
		cond.acquire();
	}

	public function tryAcquire():Bool {
		return cond.acquire(false);
	}

	public function release():Void {
		cond.release();
	}

	public function wait():Void {
		cond.wait();
	}

	public function timedWait(timeout:Float):Bool {
		final t = timeout;
		// larger values than TIMEOUT_MAX (about 49 days on Windows) raise an OverflowError
		final max = Math.min(1e9, python.lib.Threading.TIMEOUT_MAX);
		final capped = t > max;
		final signaled = cond.wait(if (!(t > 0)) 0.0 else if (capped) max else t);
		// a capped wait did not reach the requested timeout, so it counts as a spurious wakeup
		return signaled || capped;
	}

	public function signal():Void {
		cond.notify();
	}

	public function broadcast():Void {
		cond.notify_all();
	}
}
