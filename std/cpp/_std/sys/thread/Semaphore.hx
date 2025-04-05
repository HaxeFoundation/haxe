package sys.thread;

@:coreApi
class Semaphore {
	var m:Dynamic;

	public function new(value:Int) {
		m = untyped __global__.__hxcpp_semaphore_create(value);
	}

	public function acquire():Void {
		untyped __global__.__hxcpp_semaphore_acquire(m);
	}

	public function tryAcquire(?timeout:Float):Bool {
		return untyped __global__.__hxcpp_semaphore_try_acquire(m, timeout == null ? 0 : (timeout:Float));
	}

	public function release():Void {
		untyped __global__.__hxcpp_semaphore_release(m);
	}
}
