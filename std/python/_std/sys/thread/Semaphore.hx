package sys.thread;

import python.lib.threading.Semaphore as NativeSemaphore;

@:coreApi
class Semaphore {
	final semaphore:NativeSemaphore;

	public function new(value:Int):Void {
		this.semaphore = new NativeSemaphore(value);
	}

	public function acquire():Void {
		semaphore.acquire();
	}

	public function tryAcquire(?timeout:Float):Bool {
		return timeout == null ? semaphore.acquire(false) : semaphore.acquire(true, timeout);
	}

	public function release():Void {
		semaphore.release();
	}
}
