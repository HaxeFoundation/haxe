package sys.thread;

@:coreApi class Semaphore {
	public function new(value:Int):Void {}

	public function acquire():Void {}

	public function tryAcquire(?timeout:Float):Bool {
		return false;
	}

	public function release():Void {}
}
