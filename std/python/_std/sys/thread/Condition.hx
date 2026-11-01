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

	public function wait(?timeout:Float):Bool {
		if (timeout == null) {
			cond.wait();
			return true;
		}
		final t:Float = timeout;
		// larger values than TIMEOUT_MAX raise an OverflowError
		final max = Math.min(1e9, python.lib.Threading.TIMEOUT_MAX);
		return cond.wait(if (!(t > 0)) 0.0 else if (t > max) max else t);
	}

	public function signal():Void {
		cond.notify();
	}

	public function broadcast():Void {
		cond.notify_all();
	}
}
