package sys.thread;

@:coreApi class Condition {
	final cond:eval.luv.Condition;
	final mutex:eval.luv.Mutex;

	public function new():Void {
        cond = eval.luv.Condition.init().resolve();
        mutex= eval.luv.Mutex.init(true).resolve();
    }

	public function acquire():Void {
        mutex.lock();
    }

	public function tryAcquire():Bool {
        return mutex.tryLock().isOk();
    }

	public function release():Void {
        mutex.unlock();
    }

	public function wait():Void {
        cond.wait(mutex);
    }

	public function signal():Void {
        cond.signal();
    }

	public function broadcast():Void {
        cond.broadcast();
    }
}
