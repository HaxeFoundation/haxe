package sys.thread;

@:coreApi class Semaphore {
	var s:Dynamic;

	public function new(value:Int):Void {
		s = semaphore_create(value);
	}

	public function acquire():Void {
		semaphore_acquire(s);
	}

	public function tryAcquire(?timeout:Float):Bool {
		return semaphore_try_acquire(s, timeout);
	}

	public function release():Void {
		semaphore_release(s);
	}

	static var semaphore_create = neko.Lib.loadLazy("std", "semaphore_create", 1);
	static var semaphore_try_acquire = neko.Lib.loadLazy("std", "semaphore_try_acquire", 2);
	static var semaphore_acquire = neko.Lib.loadLazy("std", "semaphore_acquire", 1);
	static var semaphore_release = neko.Lib.loadLazy("std", "semaphore_release", 1);
}
