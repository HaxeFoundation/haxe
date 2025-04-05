package sys.thread;

@:coreApi
class Condition {
	var c:Dynamic;
	public function new():Void {
		c = untyped __global__.__hxcpp_condition_create();
	}

	public function acquire():Void {
		untyped __global__.__hxcpp_condition_acquire(c);
	}

	public function tryAcquire():Bool {
		return untyped __global__.__hxcpp_condition_try_acquire(c);
	}

	public function release():Void {
		untyped __global__.__hxcpp_condition_release(c);
	}

	public function wait():Void {
		untyped __global__.__hxcpp_condition_wait(c);
	}

	public function signal():Void {
		untyped __global__.__hxcpp_condition_signal(c);
	}

	public function broadcast():Void {
		untyped __global__.__hxcpp_condition_broadcast(c);
	}
}
