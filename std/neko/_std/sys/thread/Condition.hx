package sys.thread;

class Condition {
	var c:Dynamic;

	/**
		Incremented by `signal` and `broadcast` while the mutex is held, so that
		the polling fallback of `wait` can detect notifications.
	**/
	var generation:Int = 0;

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

	public function wait(?timeout:Float):Bool {
		if (timeout == null) {
			cond_wait(c);
			return true;
		}
		if (cond_timed_wait != null) {
			return cond_timed_wait(c, timeout);
		}
		return pollingWait(timeout);
	}

	/**
		Fallback for Neko versions without `cond_timed_wait` (2.4.1 and older).
	**/
	function pollingWait(timeout:Float):Bool {
		if (!(timeout > 0)) {
			return false;
		}
		final deadline = Sys.time() + timeout;
		final startGeneration = generation;
		var sleep = 0.0001;
		while (true) {
			cond_release(c);
			Sys.sleep(sleep);
			cond_acquire(c);
			if (generation != startGeneration) {
				return true;
			}
			final remaining = deadline - Sys.time();
			if (remaining <= 0) {
				return false;
			}
			sleep = Math.min(Math.min(sleep * 2, 0.01), remaining);
		}
	}

	public function signal():Void {
		generation++;
		cond_signal(c);
	}

	public function broadcast():Void {
		generation++;
		cond_broadcast(c);
	}

	static var cond_create = neko.Lib.loadLazy("std", "cond_create", 0);
	static var cond_try_acquire = neko.Lib.loadLazy("std", "cond_try_acquire", 1);
	static var cond_acquire = neko.Lib.loadLazy("std", "cond_acquire", 1);
	static var cond_release = neko.Lib.loadLazy("std", "cond_release", 1);
	static var cond_wait = neko.Lib.loadLazy("std", "cond_wait", 1);
	static var cond_signal = neko.Lib.loadLazy("std", "cond_signal", 1);
	static var cond_broadcast = neko.Lib.loadLazy("std", "cond_broadcast", 1);
	static var cond_timed_wait:Null<Dynamic> = try neko.Lib.load("std", "cond_timed_wait", 2) catch (_:Dynamic) null;
}
