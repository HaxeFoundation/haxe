package sys.thread;

#if doc_gen
@:coreApi extern class Condition {
	function new():Void;

	public function acquire():Void;

	public function tryAcquire():Bool;

	public function release():Void;

	public function wait():Void;

	public function signal():Void;

	public function broadcast():Void;
}
#else
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
#end
