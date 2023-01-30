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

	public function signal():Void {
		cond.notify();
	}

	public function broadcast():Void {
		cond.notify_all();
	}
}
