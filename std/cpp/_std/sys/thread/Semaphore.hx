package sys.thread;

@:include("hx/thread/CountingSemaphore.hpp")
@:cpp.ManagedType({ namespace : [ "hx", "thread" ], flags : [ StandardNaming ] })
private extern class CountingSemaphore {
	function new(value:Int):Void;

	public function acquire():Void;
	public function release():Void;
	public function tryAcquire(timeout:Null<Float>):Bool;
}

@:coreApi
class Semaphore {
	final s:CountingSemaphore;

	public function new(value:Int) {
		s = new CountingSemaphore(value);
	}

	public function acquire():Void {
		s.acquire();
	}

	public function tryAcquire(?timeout:Float):Bool {
		return s.tryAcquire(timeout);
	}

	public function release():Void {
		s.release();
	}
}
