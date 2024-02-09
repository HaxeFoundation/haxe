package sys.thread;

import java.util.concurrent.TimeUnit;

@:coreApi
@:native('haxe.java.vm.Semaphore')
class Semaphore {
	final native:java.util.concurrent.Semaphore;

	public function new(value:Int):Void {
		this.native = new java.util.concurrent.Semaphore(value);
	}

	public function acquire():Void {
		native.acquire();
	}

	public function tryAcquire(?timeout:Float):Bool {
		return timeout == null ? native.tryAcquire() : native.tryAcquire(haxe.Int64.fromFloat(timeout * 1000000000),TimeUnit.NANOSECONDS);
	}

	public function release():Void {
		native.release();
	}
}
