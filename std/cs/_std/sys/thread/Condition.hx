package sys.thread;

import cs.system.threading.Monitor;

@:coreApi
@:access(sys.thread.Mutex)
class Condition {
	final object:cs.system.Object;

	public function new():Void {
		this.object = new cs.system.Object();
	}

	public function acquire():Void {
		Monitor.Enter(object);
	}

	public function tryAcquire():Bool {
		return Monitor.TryEnter(object);
	}

	public function release():Void {
		Monitor.Exit(object);
	}

	public function wait():Void {
		Monitor.Wait(object);
	}

	/*
		public function timedWait(timeout:Float):Bool {
			return Monitor.Wait(object, Std.int(timeout * 1000));
		}
	 */
	public function signal():Void {
		Monitor.Pulse(object);
	}

	public function broadcast():Void {
		Monitor.PulseAll(object);
	}
}
