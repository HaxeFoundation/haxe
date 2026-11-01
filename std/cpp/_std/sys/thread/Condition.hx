package sys.thread;

@:include("hx/thread/ConditionVariable.hpp")
@:cpp.ManagedType({ namespace : [ "hx", "thread" ], flags : [ StandardNaming ] })
private extern class ConditionVariable {
	function new():Void;

	public function acquire():Void;
	public function tryAcquire():Bool;
	public function release():Void;
	public function wait():Void;
	public function timedWait(seconds:Float):Bool;
	public function signal():Void;
	public function broadcast():Void;
}

@:coreApi
class Condition {
	final c:ConditionVariable;

	public function new():Void {
		c = new ConditionVariable();
	}

	public function acquire():Void {
		c.acquire();
	}

	public function tryAcquire():Bool {
		return c.tryAcquire();
	}

	public function release():Void {
		c.release();
	}

	public function wait(?timeout:Float):Bool {
		if (timeout == null) {
			c.wait();
			return true;
		}
		return c.timedWait(timeout);
	}

	public function signal():Void {
		c.signal();
	}

	public function broadcast():Void {
		c.broadcast();
	}
}
