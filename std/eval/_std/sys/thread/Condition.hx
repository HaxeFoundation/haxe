package sys.thread;

@:coreApi class Condition {
	public function new():Void {}

	public function acquire():Void {}

	public function tryAcquire():Bool {
		return false;
	}

	public function release():Void {}

	public function wait():Void {}

	public function signal():Void {}

	public function broadcast():Void {}
}
