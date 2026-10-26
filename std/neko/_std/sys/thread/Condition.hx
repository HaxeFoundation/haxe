package sys.thread;

class Condition {
	var c:Dynamic;

	public function new():Void {
		c = cond_create();
	}

	public function acquire():Void {
		cond_acquire(c);
	}

	public function tryAcquire():Bool {
		return cond_try_acquire(c);
	}

	public function release():Void {
		cond_release(c);
	}

	public function wait():Void {
		cond_wait(c);
	}

	public function signal():Void {
		cond_signal(c);
	}

	public function broadcast():Void {
		cond_broadcast(c);
	}

	static var cond_create = neko.Lib.loadLazy("std", "cond_create", 0);
	static var cond_try_acquire = neko.Lib.loadLazy("std", "cond_try_acquire", 1);
	static var cond_acquire = neko.Lib.loadLazy("std", "cond_acquire", 1);
	static var cond_release = neko.Lib.loadLazy("std", "cond_release", 1);
	static var cond_wait = neko.Lib.loadLazy("std", "cond_wait", 1);
	static var cond_signal = neko.Lib.loadLazy("std", "cond_signal", 1);
	static var cond_broadcast = neko.Lib.loadLazy("std", "cond_broadcast", 1);
}
