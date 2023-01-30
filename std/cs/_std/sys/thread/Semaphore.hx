package sys.thread;

@:noDoc
@:coreApi
class Semaphore {
	final native:cs.system.threading.Semaphore;

	public function new(value:Int):Void {
		this.native = new cs.system.threading.Semaphore(value, 0x7FFFFFFF);
	}

	public function acquire():Void {
		native.WaitOne();
	}

	public function tryAcquire(?timeout:Float):Bool {
		return native.WaitOne(timeout == null ? 0 : Std.int(timeout * 1000));
	}

	public function release():Void {
		native.Release();
	}
}
