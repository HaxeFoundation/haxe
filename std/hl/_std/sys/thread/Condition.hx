package sys.thread;

@:noDoc
abstract Condition(hl.Abstract<"hl_condition">) {
	public function new():Void {
		this = alloc();
	}

	@:hlNative("std", "condition_acquire")
	public function acquire():Void {}

	@:hlNative("std", "condition_try_acquire")
	public function tryAcquire():Bool {
		return false;
	}

	@:hlNative("std", "condition_release")
	public function release():Void {}

	@:hlNative("std", "condition_wait")
	public function wait():Void {}

	@:hlNative("std", "condition_signal")
	public function signal():Void {}

	@:hlNative("std", "condition_broadcast")
	public function broadcast():Void {}

	@:hlNative("std", "condition_alloc")
	static function alloc():hl.Abstract<"hl_condition"> {
		return null;
	}
}
