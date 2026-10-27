package sys.thread;

@:coreApi class Semaphore {
	var s:Dynamic;

	public function new(value:Int):Void {
		s = lock_create(value);
	}

	public function acquire():Void {
		lock_wait(s);
	}

	public function tryAcquire(?timeout:Float):Bool {
		return lock_wait(s, timeout ?? 0.);
	}

	public function release():Void {
		lock_release(s);
	}

	static var lock_create = neko.Lib.loadLazy("std", "lock_create", 1);
	static var lock_release = neko.Lib.load("std", "lock_release", 1);
	static var lock_wait:(Dynamic, ?Float) -> Bool = neko.Lib.load("std", "lock_wait", 2);
}
